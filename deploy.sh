#!/bin/bash

bundle exec rake clean gen_deploy
rm -rf docs/*
cp -R public/* docs/

git add docs
git commit -am 'deploy'
git push

./clear-cloudflare.sh
