#!/bin/bash

filename=$(basename "$1")
dirname="${filename%.*}"
ghcjs -O2 $dirname

cd $dirname.jsexe
if [ -d "$dirname" ] 
    then rm -rf $dirname
    fi
    
mkdir $dirname

ccjs all.js --compilation_level=ADVANCED_OPTIMIZATIONS --externs=node > $dirname/all.min.js 
cp ../index.html.serve $dirname/index.html

scp -r $dirname lambdasistemi.net:public/ghcjs


