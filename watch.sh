#!/bin/bash
while sleep 0.1; do 
    rg bb templates posts --files -t css -t clojure -t markdown -t html \
        | entr -d -s 'rm -rf output/*; time /opt/homebrew/bin/bb render';
done
