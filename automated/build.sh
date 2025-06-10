#!/usr/bin/env bash

# shellcheck disable=SC2086,SC1091,SC2001
# SC2086: We actually want some word splitting to happen
# SC1091: Secrets are sourced from a file that doesn't exist in the tree.
# SC2001: Pattern substitution is too hard to use; don't recommend it.

set -eu +x -o pipefail

ROOT=$(cd "$(dirname $0)" ; pwd)
TARGET=$1

# Home on the container
: "${C_HOME:=$HOME}"

# User to run as on the container
: "${USERID:=$(id -u)}"

source work/aws.sh

# For nightly-YYYY-MM-DD, tag should be nightly
# For lts-X.Y, tag should be ltsX
SHORTNAME=$(echo $TARGET | cut -d- -f 1)
if [ $SHORTNAME = "lts" ]
then
    TAG=$(echo $TARGET | sed 's@^lts-\([0-9]*\)\.[0-9]*@lts\1@')
    if [ -n "${NOPLAN:-}" ]
    then
        echo '* DO NOT EDIT work/ files: commit to lts-haskell/build-constraints! *'
        exit 1
    fi
else
    TAG=$SHORTNAME
fi
WORKDIR=$ROOT/work/$TAG

IMAGE=ghcr.io/commercialhaskell/stackage/build:$TAG

docker pull $IMAGE

PANTRY_DIR=$ROOT/work/stack/pantry
STACK_DIR=$ROOT/work/stack
DOT_STACKAGE_DIR=$ROOT/work/dot-stackage
# ssh key is used for committing snapshots (and their constraints) to Github
SSH_DIR=$ROOT/work/ssh

mkdir -p \
	"$PANTRY_DIR" \
	"$STACK_DIR" \
	"$DOT_STACKAGE_DIR" \
	"$WORKDIR" \
	"$SSH_DIR"

GITCONFIG=$ROOT/work/gitconfig
cat >$GITCONFIG <<EOF
[user]
	email = michael+stackage-build@fpcomplete.com
	name = Stackage Build host
EOF

HACKAGE_CREDS=$ROOT/work/hackage-creds

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

mkdir -p $ROOT/work/bin
BINDIR=$(cd $ROOT/work/bin ; pwd)
(
cd $BINDIR
rm -f curator stack -- *.bz2

if [ $SHORTNAME = "lts" ]; then
    # drop for lts24
    curl -L "https://github.com/commercialhaskell/curator/releases/download/commit-54cc5a95a7e29550e0fd7a48b24ddad105d223b2/curator.bz2" | bunzip2 > curator
else
    # needed for ghc-9.10
    curl -L "https://github.com/commercialhaskell/curator/releases/download/commit-6689440033b12182c0853bdd23880a84849eb6b2/curator.bz2" | bunzip2 > curator
fi
chmod +x curator

if [ $SHORTNAME = "lts" ]; then
    STACK_VERSION=3.1.1
else
    STACK_VERSION=3.5.1
fi
# rc url
#curl -L https://github.com/commercialhaskell/stack/releases/download/rc%2Fv${STACK_VERSION}/stack-${STACK_VERSION}-linux-x86_64-bin > stack
curl -L https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-x86_64-bin > stack
chmod +x stack

docker run --rm -v "$(pwd)"/curator:/curator -v "$(pwd)"/stack:/stack $IMAGE /bin/bash -c "
    echo -n 'curator version: '
    /curator --version
    echo -n 'stack version: '
    /stack --version
    "
)

# We share pantry directory between snapshots while the other content in .stack
# is stored separately (because e.g. Ubuntu releases between LTS and nightly
# could differ). Also the order of binds is important.
ARGS_COMMON="--rm -v $WORKDIR:$C_HOME/work -w $C_HOME/work -v $BINDIR/curator:/usr/bin/curator:ro -v /etc/passwd:/etc/passwd:ro -v /etc/group:/etc/group:ro -v $BINDIR/stack:/usr/bin/stack:ro -v $STACK_DIR:$C_HOME/.stack -v $PANTRY_DIR:$C_HOME/.stack/pantry -v $HOME/.aws/config:$C_HOME/.aws/config:ro"
ARGS_PREBUILD="$ARGS_COMMON -u $USERID -e HOME=$C_HOME -v $DOT_STACKAGE_DIR:$C_HOME/.stackage"
ARGS_BUILD="$ARGS_COMMON"
ARGS_UPLOAD="$ARGS_PREBUILD -v $HACKAGE_CREDS:/hackage-creds:ro -v $SSH_DIR:$C_HOME/.ssh:ro -v $GITCONFIG:$C_HOME/.gitconfig:ro -e AWS_ACCESS_KEY_ID=$AWS_ACCESS_KEY_ID -e AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY ${AWS_ENDPOINT_URL:+-e AWS_ENDPOINT_URL=$AWS_ENDPOINT_URL} -v $DOT_STACKAGE_DIR:/dot-stackage"

# for debugging etc
if [ -n "${2:-}" ]
then
    docker run -it $ARGS_UPLOAD $IMAGE $2
    exit 0
fi

# Make sure we actually need this snapshot. We only check this for LTS releases
# since, for nightlies, we'd like to run builds even if they are unnecessary to
# get early warning information of upcoming failures. (See the duplicate check
# below for why this is safe.)
if [ $SHORTNAME = "lts" ]
then
    docker run $ARGS_UPLOAD $IMAGE curator check-target-available --target $TARGET
fi


# Determine the new build plan
#
# * Update the package index (unless LTS)
# * Create a new plan
docker run $ARGS_PREBUILD $IMAGE /bin/bash -c "
    set -e
    if [ $SHORTNAME = 'nightly' ]; then
        curator update
    fi
    curator constraints --target $TARGET
    curator snapshot-incomplete --target $TARGET
    curator snapshot
# Do the rest of the pre-build actions:
#
# * Check that the snapshot is valid
# * Fetch and unpack all needed tarballs (the build step does not have write access to the tarball directory)
    "'
    GHCVER=$(sed -n "s/^ghc-version: \(.*\)/\1/p" constraints.yaml)
    stack setup ghc-$GHCVER --verbosity=error
    stack exec --resolver=ghc-$GHCVER curator check-snapshot
    curator unpack
    '

case $SHORTNAME in
    lts) JOBS=16 ;;
    nightly) JOBS=16 ;;
esac

if [ -e "$SHORTNAME-build.log" ]
then
    cp -p $SHORTNAME-build.log $SHORTNAME-build.log-previous
fi

# Now do the actual build. We need to first set the owner of the home directory
# correctly, so we run the command as root, change owner, and then use sudo to
# switch back to the current user
docker run $ARGS_BUILD $IMAGE nice -n 15 /bin/bash -c "
    chown $USER $HOME
    exec sudo -E -u $USER env \"HOME=$HOME\" \"PATH=\$PATH\" curator build --jobs $JOBS
    " |& tee $SHORTNAME-build.log

# Make sure we actually need this snapshot. We used to perform this check
# exclusively before building. Now we perform it after as well for the case of
# nightly, where we don't perform this check beforehand. This is also slightly
# safer, in case someone else already uploaded a specific snapshot while we
# were building.
docker run $ARGS_UPLOAD $IMAGE curator check-target-available --target $TARGET

# Successful build, so we need to:
#
# * Upload the docs to S3
# * Upload the new snapshot .yaml file to the appropriate Github repo, also upload its constraints
date
docker run $ARGS_UPLOAD -e "CURATOR_AWS_OPTIONS=--only-show-errors" $IMAGE /bin/bash -c "
    set -e
    ulimit -n hard
    curator upload-docs --target $TARGET ${DOCS_BUCKET:+--bucket $DOCS_BUCKET}
    curator upload-github --target $TARGET
    "
date

docker run $ARGS_UPLOAD $IMAGE curator hackage-distro --target $TARGET

# Build and push docker image fpco/stack-build & fpco/stack-build-small for current release

if [ $SHORTNAME = "lts" ]
then
    $ROOT/dockerfiles/build.sh $TARGET
    $ROOT/dockerfiles/build.sh --push $TARGET
    $ROOT/dockerfiles/build.sh --push --small $TARGET
fi

echo -n "Completed at "
date
