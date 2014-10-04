#!/bin/ksh 
echo "cc surface.c -O2 -o  ../exe/surface -lm "
cc surface.c  -O2 -o ../exe/surface -lm
