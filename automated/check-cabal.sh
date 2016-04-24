#!/usr/bin/env bash

# This script is intended for testing Cabal HEAD against a Stackage snapshot.

set -eux

ROOT=$(cd $(dirname $0) ; pwd)
TARGET=$1

# For nightly-YYYY-MM-DD, tag should be nightly
# For lts-X.Y, tag should be ltsX
SHORTNAME=$(echo $TARGET | cut -d- -f 1)
if [ $SHORTNAME = "lts" ]
then
    TAG=$(echo $TARGET | sed 's@^lts-\([0-9]*\)\.[0-9]*@lts\1@')
    PLAN_URL=https://raw.githubusercontent.com/fpco/lts-haskell/master/$TARGET.yaml
else
    TAG=$SHORTNAME
    PLAN_URL=https://raw.githubusercontent.com/fpco/stackage-nightly/master/$TARGET.yaml
fi

IMAGE=snoyberg/stackage:$TAG

PLAN_FILE=$TARGET-plan.yaml
DOCMAP_FILE=$TARGET-docmap.yaml
BUNDLE_FILE=$TARGET.bundle=

CABAL_DIR=$ROOT/cabal
STACK_DIR=$ROOT/stack
GHC_DIR=$ROOT/ghc
DOT_STACKAGE_DIR=$ROOT/dot-stackage
WORKDIR=$ROOT/$TAG/work
EXTRA_BIN_DIR=$ROOT/extra-bin

mkdir -p \
	"$CABAL_DIR" \
	"$STACK_DIR" \
	"$GHC_DIR" \
	"$DOT_STACKAGE_DIR" \
	"$WORKDIR" \
	"$EXTRA_BIN_DIR"

curl "$PLAN_URL" > $WORKDIR/$PLAN_FILE

mkdir -p $ROOT/bin
BINDIR=$(cd $ROOT/bin ; pwd)
(
cd $BINDIR
rm -f stackage-curator stackage-curator.bz2
wget https://s3.amazonaws.com/stackage-travis/stackage-curator/stackage-curator.bz2
bunzip2 stackage-curator.bz2
chmod +x stackage-curator
)

ARGS_COMMON="--rm -v $WORKDIR:/home/stackage/work -w /home/stackage/work -v $BINDIR/stackage-curator:/usr/bin/stackage-curator:ro -v /etc/passwd:/etc/passwd:ro -v /etc/group:/etc/group:ro -v $EXTRA_BIN_DIR:/home/stackage/bin:ro"
ARGS_PREBUILD="$ARGS_COMMON -u $USER -v $CABAL_DIR:/home/stackage/.cabal -v $STACK_DIR:/home/stackage/.stack -v $GHC_DIR:/home/stackage/.ghc -v $DOT_STACKAGE_DIR:/home/stackage/.stackage"
ARGS_BUILD="$ARGS_COMMON -v $CABAL_DIR:/home/stackage/.cabal:ro -v $STACK_DIR:/home/stackage/.stack:ro -v $GHC_DIR:/home/stackage/.ghc:ro"

# Get latest stack
curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C $EXTRA_BIN_DIR '*/stack'

# Do all of the pre-build actions:
#
# * Update the package index
# * Fetch all needed tarballs (the build step does not have write access to the tarball directory)
# * Do a single unpack to create the package index cache (again due to directory perms)
docker run $ARGS_PREBUILD $IMAGE /bin/bash -c "/home/stackage/bin/stack update && stackage-curator fetch --plan-file $PLAN_FILE && cd /tmp && /home/stackage/bin/stack unpack random"

# Now do the actual build. We need to first set the owner of the home directory
# correctly, so we run the command as root, change owner, and then use sudo to
# switch back to the current user
docker run $ARGS_BUILD $IMAGE /bin/bash -c "chown $USER /home/stackage && sudo -E -u $USER env \"PATH=\$PATH:/home/stackage/bin\" stackage-curator make-bundle --plan-file $PLAN_FILE --docmap-file $DOCMAP_FILE --bundle-file $BUNDLE_FILE --target $TARGET --cabal-from-head"
