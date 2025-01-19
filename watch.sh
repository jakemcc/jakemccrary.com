#!/bin/bash
while sleep 0.1; do rg bb templates posts --files -t clojure -t markdown -t html | entr -d time /opt/homebrew/bin/bb render; done
