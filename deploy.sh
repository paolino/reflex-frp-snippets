#!/bin/bash

filename=$(basename "$1")
dirname="${filename%.*}"

echo "compiling $1"

ghcjs -O2 $dirname > /dev/null

cd $dirname.jsexe
if [ -d "$dirname" ] 
    then rm -rf $dirname
    fi
    
mkdir $dirname

echo "compressing"

ccjs all.js --compilation_level=ADVANCED_OPTIMIZATIONS --externs=node > $dirname/all.min.js &> /dev/null

cp ../index.html.serve $dirname/index.html > /dev/null

echo "uploading"
scp -r $dirname lambdasistemi.net:public/ghcjs > /dev/null



