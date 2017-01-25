#!/usr/bin/env bash

set -eu +x

ROOT=$(cd $(dirname $0) ; pwd)
TARGET=$1

source aws.sh

# For nightly-YYYY-MM-DD, tag should be nightly
# For lts-X.Y, tag should be ltsX
SHORTNAME=$(echo $TARGET | cut -d- -f 1)
if [ $SHORTNAME = "lts" ]
then
    TAG=$(echo $TARGET | sed 's@^lts-\([0-9]*\)\.[0-9]*@lts\1@')
else
    TAG=$SHORTNAME
fi

IMAGE=snoyberg/stackage:$TAG

PLAN_FILE=current-plan.yaml
DOCMAP_FILE=current-docmap.yaml
BUNDLE_FILE=current.bundle

CABAL_DIR=$ROOT/cabal
STACK_DIR=$ROOT/stack
GHC_DIR=$ROOT/ghc
DOT_STACKAGE_DIR=$ROOT/dot-stackage
WORKDIR=$ROOT/$TAG/work
EXTRA_BIN_DIR=$ROOT/extra-bin
SSH_DIR=$ROOT/ssh-$SHORTNAME

mkdir -p \
	"$CABAL_DIR" \
	"$STACK_DIR" \
	"$GHC_DIR" \
	"$DOT_STACKAGE_DIR" \
	"$WORKDIR" \
	"$EXTRA_BIN_DIR" \
	"$SSH_DIR"

GITCONFIG=$ROOT/gitconfig
cat >$GITCONFIG <<EOF
[user]
	email = michael+stackage-build@fpcomplete.com
	name = Stackage Build host
EOF

cat >$SSH_DIR/known_hosts <<EOF
|1|Qn0iij8BnxGZXbyFSozS9zWkH+Q=|YrKKNp2KHO3/oc4UBFIe1zOvhDc= ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==
|1|RxBEt2ljiEppr019szMIhbY12m0=|0FZ2Oji1LphRbPLLEQhFzTmL69I= ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==
EOF

AUTH_TOKEN=$ROOT/auth-token
HACKAGE_CREDS=$ROOT/hackage-creds

function require_400_file {
    if [ ! -f "$1" ]
    then
        echo File not found: "$1"
        exit 1
    fi

    chmod 400 "$1"
}

require_400_file "$SSH_DIR/id_rsa"
require_400_file "$AUTH_TOKEN"
require_400_file "$HACKAGE_CREDS"

mkdir -p $ROOT/bin
BINDIR=$(cd $ROOT/bin ; pwd)
(
cd $BINDIR
rm -f stackage-curator stackage-curator.bz2
wget https://s3.amazonaws.com/stackage-travis/stackage-curator/stackage-curator.bz2
bunzip2 stackage-curator.bz2
chmod +x stackage-curator
./stackage-curator --version
)

ARGS_COMMON="--rm -v $WORKDIR:$HOME/work -w $HOME/work -v $BINDIR/stackage-curator:/usr/bin/stackage-curator:ro -v /etc/passwd:/etc/passwd:ro -v /etc/group:/etc/group:ro -v $EXTRA_BIN_DIR/stack:/usr/bin/stack:ro"
ARGS_PREBUILD="$ARGS_COMMON -u $USER -e HOME=$HOME -v $CABAL_DIR:$HOME/.cabal -v $STACK_DIR:$HOME/.stack -v $GHC_DIR:$HOME/.ghc -v $DOT_STACKAGE_DIR:$HOME/.stackage"
ARGS_BUILD="$ARGS_COMMON -v $CABAL_DIR:$HOME/.cabal:ro -v $STACK_DIR:$HOME/.stack:ro -v $GHC_DIR:$HOME/.ghc:ro"
ARGS_UPLOAD="$ARGS_COMMON -u $USER -e HOME=$HOME -e AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID -e AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY -v $AUTH_TOKEN:/auth-token:ro -v $HACKAGE_CREDS:/hackage-creds:ro -v $DOT_STACKAGE_DIR:$HOME/.stackage -v $SSH_DIR:$HOME/.ssh:ro -v $GITCONFIG:$HOME/.gitconfig:ro -v $CABAL_DIR:$HOME/.cabal:ro -v $STACK_DIR:$HOME/.stack:ro"

# Make sure we actually need this snapshot. We only check this for LTS releases
# since, for nightlies, we'd like to run builds even if they are unnecessary to
# get early warning information of upcoming failures. (See the duplicate check
# below for why this is safe.)
if [ $SHORTNAME = "lts" ]
then
  docker run $ARGS_UPLOAD $IMAGE /bin/bash -c "exec stackage-curator check-target-available --target $TARGET"
fi

# Get latest stack
curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C $EXTRA_BIN_DIR '*/stack'

# Determine the new build plan unless NOPLAN is set
#
# * Update the package index (unless LTS)
# * Create a new plan
if [ "${NOPLAN:-}x" = "x" ]
then
    if [ $SHORTNAME = "lts" ]
    then
        docker run $ARGS_PREBUILD $IMAGE /bin/bash -c "exec stackage-curator create-plan --plan-file $PLAN_FILE --target $TARGET ${CONSTRAINTS:-}"
    else
        docker run $ARGS_PREBUILD $IMAGE /bin/bash -c "stack update && exec stackage-curator create-plan --plan-file $PLAN_FILE --target $TARGET ${CONSTRAINTS:-}"
    fi
fi

# Do the rest of the pre-build actions:
#
# * Check that the plan is valid
# * Fetch all needed tarballs (the build step does not have write access to the tarball directory)
# * Do a single unpack to create the package index cache (again due to directory perms)
docker run $ARGS_PREBUILD $IMAGE /bin/bash -c "stackage-curator check --plan-file $PLAN_FILE && stackage-curator fetch --plan-file $PLAN_FILE && cd /tmp && exec stack unpack random"

# Now do the actual build. We need to first set the owner of the home directory
# correctly, so we run the command as root, change owner, and then use sudo to
# switch back to the current user
docker run $ARGS_BUILD $IMAGE nice -n 15 /bin/bash -c "chown $USER $HOME && exec sudo -E -u $USER env \"HOME=$HOME\" \"PATH=\$PATH\" stackage-curator make-bundle --jobs 4 --plan-file $PLAN_FILE --docmap-file $DOCMAP_FILE --bundle-file $BUNDLE_FILE --target $TARGET"

# Make sure we actually need this snapshot. We used to perform this check
# exclusively before building. Now we perform it after as well for the case of
# nightly, where we don't perform this check beforehand. This is also slightly
# safer, in case someone else already uploaded a specific snapshot while we
# were building.
docker run $ARGS_UPLOAD $IMAGE /bin/bash -c "exec stackage-curator check-target-available --target $TARGET"

# Successful build, so we need to:
#
# * Upload the docs to S3
# * Upload the 00-index.tar file to S3 (TODO: this is probably no longer necessary, since snapshots never modify .cabal files)
# * Upload the new plan .yaml file to the appropriate Github repo
# * Register as a new Hackage distro
docker run $ARGS_UPLOAD $IMAGE /bin/bash -c "stackage-curator upload-docs --target $TARGET --bundle-file $BUNDLE_FILE && stackage-curator upload-index --plan-file $PLAN_FILE --target $TARGET && stackage-curator upload-github --plan-file $PLAN_FILE --docmap-file $DOCMAP_FILE --target $TARGET && exec stackage-curator hackage-distro --plan-file $PLAN_FILE --target $TARGET"

date
