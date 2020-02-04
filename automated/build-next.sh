#!/usr/bin/env bash

set -eu +x -o pipefail

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

IMAGE=commercialhaskell/stackage:$TAG

CABAL_DIR=$ROOT/cabal
PANTRY_DIR=$ROOT/pantry
STACK_DIR=$ROOT/stack-$TAG
GHC_DIR=$ROOT/ghc
DOT_STACKAGE_DIR=$ROOT/dot-stackage
WORKDIR=$ROOT/$TAG/work
# ssh key is used for committing snapshots (and their constraints) to Github
SSH_DIR=$ROOT/ssh
USERID=$(id -u)

mkdir -p \
	"$CABAL_DIR" \
	"$PANTRY_DIR" \
	"$STACK_DIR" \
	"$GHC_DIR" \
	"$DOT_STACKAGE_DIR" \
	"$WORKDIR" \
	"$SSH_DIR"

GITCONFIG=$ROOT/gitconfig
cat >$GITCONFIG <<EOF
[user]
	email = michael+stackage-build@fpcomplete.com
	name = Stackage Build host
EOF

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
require_400_file "$HACKAGE_CREDS"

mkdir -p $ROOT/bin
BINDIR=$(cd $ROOT/bin ; pwd)
(
# See etc/curator-exes/README.md
CURATOR_EXES=84f6e06e11e1bcdab6ec1a302d40213e406748e64ae455bd4ed09a205651a7fd
cd $BINDIR
rm -f curator stack *.bz2
wget "https://s3.amazonaws.com/download.fpcomplete.com/curator-exes/curator-exes-$CURATOR_EXES.tar.bz2"
tar xf "curator-exes-$CURATOR_EXES.tar.bz2"
echo -n "curator version: "
docker run --rm -v $(pwd)/curator:/exe $IMAGE /exe --version
echo -n "stack version: "
docker run --rm -v $(pwd)/stack:/exe $IMAGE /exe --version
)

# We share pantry directory between snapshots while the other content in .stack
# is stored separately (because e.g. Ubuntu releases between LTS and nightly
# could differ). Also the order of binds is important.
ARGS_COMMON="--rm -v $WORKDIR:$HOME/work -w $HOME/work -v $BINDIR/curator:/usr/bin/curator:ro -v /etc/passwd:/etc/passwd:ro -v /etc/group:/etc/group:ro -v $BINDIR/stack:/usr/bin/stack:ro -v $STACK_DIR:$HOME/.stack -v $PANTRY_DIR:$HOME/.stack/pantry"
ARGS_PREBUILD="$ARGS_COMMON -u $USERID -e HOME=$HOME -v $CABAL_DIR:$HOME/.cabal -v $GHC_DIR:$HOME/.ghc -v $DOT_STACKAGE_DIR:$HOME/.stackage"
ARGS_BUILD="$ARGS_COMMON -v $CABAL_DIR:$HOME/.cabal:ro -v $GHC_DIR:$HOME/.ghc:ro"
# instance-data is an undocumented feature of S3 used by amazonka,
# see https://github.com/brendanhay/amazonka/issues/271
ARGS_UPLOAD="$ARGS_COMMON -u $USERID -e HOME=$HOME -v $HACKAGE_CREDS:/hackage-creds:ro -v $DOT_STACKAGE_DIR:$HOME/.stackage -v $SSH_DIR:$HOME/.ssh:ro -v $GITCONFIG:$HOME/.gitconfig:ro -v $CABAL_DIR:$HOME/.cabal:ro -e AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID -e AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY"

# Make sure we actually need this snapshot. We only check this for LTS releases
# since, for nightlies, we'd like to run builds even if they are unnecessary to
# get early warning information of upcoming failures. (See the duplicate check
# below for why this is safe.)
if [ $SHORTNAME = "lts" ]
then
  docker run $ARGS_UPLOAD $IMAGE /bin/bash -c "exec curator check-target-available --target $TARGET"
fi


# Determine the new build plan unless NOPLAN is set
#
# * Update the package index (unless LTS)
# * Create a new plan
if [ "${NOPLAN:-}x" = "x" ]
then
    if [ $SHORTNAME = "lts" ]
    then
        docker run $ARGS_PREBUILD $IMAGE /bin/bash -c "curator constraints --target $TARGET && curator snapshot-incomplete --target $TARGET && curator snapshot"
    else
        docker run $ARGS_PREBUILD $IMAGE /bin/bash -c "curator update && curator constraints --target $TARGET && curator snapshot-incomplete --target $TARGET && curator snapshot"
    fi
else
    docker run $ARGS_PREBUILD $IMAGE /bin/bash -c "curator snapshot"
fi


# Do the rest of the pre-build actions:
#
# * Check that the snapshot is valid
# * Fetch and unpack all needed tarballs (the build step does not have write access to the tarball directory)
docker run $ARGS_PREBUILD $IMAGE /bin/bash -c 'GHCVER=$(sed -n "s/^ghc-version: \(.*\)/\1/p" constraints.yaml) && stack setup ghc-$GHCVER --verbosity=error && stack exec --resolver=ghc-$GHCVER curator check-snapshot && curator unpack'

case $SHORTNAME in
    lts) JOBS=1 ;;
    nightly) JOBS=2 ;;
esac

# Now do the actual build. We need to first set the owner of the home directory
# correctly, so we run the command as root, change owner, and then use sudo to
# switch back to the current user
docker run $ARGS_BUILD $IMAGE nice -n 15 /bin/bash -c "chown $USER $HOME && exec sudo -E -u $USER env \"HOME=$HOME\" \"PATH=\$PATH\" curator build --jobs $JOBS" 2>&1 | tee "$SHORTNAME-build.log"

# Make sure we actually need this snapshot. We used to perform this check
# exclusively before building. Now we perform it after as well for the case of
# nightly, where we don't perform this check beforehand. This is also slightly
# safer, in case someone else already uploaded a specific snapshot while we
# were building.
docker run $ARGS_UPLOAD $IMAGE /bin/bash -c "exec curator check-target-available --target $TARGET"

# Successful build, so we need to:
#
# * Upload the docs to S3
# * Upload the new snapshot .yaml file to the appropriate Github repo, also upload its constraints
# * Register as a new Hackage distro (currently disabled)
docker run $ARGS_UPLOAD $IMAGE /bin/bash -c "curator upload-docs --target $TARGET && curator upload-github --target $TARGET && exec curator hackage-distro --target $TARGET"
# information about the new snapshots on Hackage

$BINDIR/curator legacy-bulk --stackage-snapshots dot-stackage/curator/stackage-snapshots/ --lts-haskell dot-stackage/curator/lts-haskell/ --stackage-nightly dot-stackage/curator/stackage-nightly/

(

if [ $SHORTNAME = "lts" ]
then
    cd dot-stackage/curator/lts-haskell
else
    cd dot-stackage/curator/stackage-nightly
fi

git add *.yaml
git diff-index --quiet HEAD && echo No changes && exit 0
git config user.name "Stackage build server"
git config user.email "michael@snoyman.com"
git commit -a -m "More conversions $(date)"

if [ $SHORTNAME = "lts" ]
then
    GIT_SSH_COMMAND="ssh -i $ROOT/ssh-lts/id_rsa" git push origin master
else
    GIT_SSH_COMMAND="ssh -i $ROOT/ssh-nightly/id_rsa" git push origin master
fi

)

echo -n "Completed at "
date
