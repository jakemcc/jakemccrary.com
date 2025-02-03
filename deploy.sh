#!/bin/bash

if [ -z ${WAS_SOURCED+x} ]; then
    echo "source Envfile"
    exit 1
fi

rm -rf public/*
bundle exec rake clean gen_deploy
rm -rf docs/*
cp -R public/* docs/

git add docs
git commit -am 'deploy'
git push

./clear-cloudflare.sh
