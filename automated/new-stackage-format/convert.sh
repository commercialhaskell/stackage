#!/usr/bin/env bash

set -eux

if [[ ! -f convert ]]
then
    curl https://s3.amazonaws.com/www.snoyman.com/convert-old-stackage-c9c4d06b31cb7aafedd23aa316b8008c45e4d4dd.bz2 > convert.bz2
    chmod +x convert.bz2
    bunzip2 convert.bz2
fi

cd $(dirname ${BASH_SOURCE[0]})

for d in lts-haskell stackage-nightly stackage-snapshots
do
    if [[ ! -d "$d" ]]
    then
        git clone https://github.com/commercialhaskell/$d
    else
        (cd "$d" && git pull || echo "Git pull failed, ignoring")
    fi
done

(
export HOME=$(pwd)
./convert
)

cd stackage-snapshots
git add lts nightly
git diff-index --quiet HEAD && echo No changes && exit 0
git config user.name "Stackage build server"
git config user.email "michael@snoyman.com"
git commit -m "More conversions $(date)"
GIT_SSH_COMMAND='ssh -i ../../ssh-lts/id_rsa' git push git@github.com:commercialhaskell/stackage-snapshots master
