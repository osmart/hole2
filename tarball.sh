#!/bin/bash
# command used to packup distribution tar ball
UNAME=`uname`-`uname -m` # for instance Linux-x86_64
FNAME=hole2/hole2
FNAME+=-$HoleBuild # for instance NotForProfit
FNAME+=-$HoleVersion # for instance 2.004
FNAME+=-$UNAME  # for instance Linux-x86_64
FNAME+=.tar.gz 
echo "tarball name $FNAME"
cd ..
tar cvzf $FNAME --exclude README.md  --exclude src \
   --exclude 'hole2/*.tar.gz' --exclude index.asciidoc --exclude .ignore \
   --exclude tarball.sh --exclude .git --exclude .gitignore --exclude 'hole2/source*' \
   hole2
