#!/bin/bash

set -ex 

rm -rf output/*
bb test
bb render
rm -rf docs/*
mkdir -p docs
cp -R output/* docs/
