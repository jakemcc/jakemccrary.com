#!/bin/bash

set -ex 

if [ -z ${WAS_SOURCED+x} ]; then
    echo "source Envfile"
    exit 1
fi

rm -rf output/*
bb test
bb render
rm -rf docs/*
cp -R output/* docs/

git add docs
git commit -am 'deploy'
# git push

# ./clear-cloudflare.sh
