#!/bin/bash
while sleep 0.5; do 
    rg bb templates source --files -t css -t clojure -t markdown -t html \
        | entr -d -s 'rm -rf output/*; time /opt/homebrew/bin/bb render --preview true';
done
