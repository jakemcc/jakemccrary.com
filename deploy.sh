#!/bin/bash

set -ex 

rm -rf output/*
bb test
bb render
rm -rf docs/*
cp -R output/* docs/

git add docs
git commit -am 'deploy'
# git push

# ./clear-cloudflare.sh
