#!/bin/bash
while sleep 0.1; do rg --files -t clojure -t markdown -t html | entr -d /opt/homebrew/bin/bb render; done
