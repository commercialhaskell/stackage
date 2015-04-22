#!/bin/bash

# Work in progress

set -e
set -x

TARGET=nightly
WORKDIR=nightly
PLAN_FILE=current-plan.yaml
BUNDLE_FILE=current.bundle

cd /home/ubuntu

mkdir -p cabal ghc $WORKDIR .stackage-curator
rm -f stackage-curator stackage-curator.bz2
wget https://s3.amazonaws.com/stackage-travis/stackage-curator/stackage-curator.bz2
bunzip2 stackage-curator.bz2
chmod +x stackage-curator

cat >gitconfig <<EOF
[user]
	email = michael+stackage-build@fpcomplete.com
	name = Stackage Build host
EOF

ARGS_COMMON="--rm -u $USER -e HOME=/home/stackage -v $(pwd)/$WORKDIR:/home/stackage/work -w /home/stackage/work -v $(pwd)/stackage-curator:/usr/local/bin/stackage-curator:ro -v /etc/passwd:/etc/passwd:ro -v /etc/group:/etc/group:ro"
ARGS_PREBUILD="$ARGS_COMMON -v $(pwd)/cabal:/home/stackage/.cabal -v $(pwd)/ghc:/home/stackage/.ghc"
ARGS_BUILD="$ARGS_COMMON -v $(pwd)/cabal:/home/stackage/.cabal:ro -v $(pwd)/ghc:/home/stackage/.ghc:ro"
ARGS_UPLOAD="$ARGS_COMMON -v /auth-token:/auth-token:ro -v /hackage-creds:/hackage-creds:ro -v $(pwd)/.stackage-curator:/home/stackage/.stackage-curator -v $(pwd)/.ssh:/home/ubuntu/.ssh:ro -v $(pwd)/gitconfig:/home/stackage/.gitconfig:ro"

docker run $ARGS_PREBUILD snoyberg/stackage /bin/bash -c "stackage-curator update && stackage-curator create-plan --plan-file $PLAN_FILE --target $TARGET && stackage-curator check --plan-file $PLAN_FILE && stackage-curator fetch --plan-file $PLAN_FILE && cabal install random cabal-install"
docker run $ARGS_BUILD snoyberg/stackage stackage-curator make-bundle --plan-file $PLAN_FILE --bundle-file $BUNDLE_FILE --target $TARGET
docker run $ARGS_UPLOAD snoyberg/stackage /bin/bash -c "stackage-curator upload --bundle-file $BUNDLE_FILE && stackage-curator hackage-distro --plan-file $PLAN_FILE --target $TARGET && stackage-curator upload-github --plan-file $PLAN_FILE --target $TARGET"
