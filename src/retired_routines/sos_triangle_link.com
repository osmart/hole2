#!/bin/sh 
echo "cc sos_triangle.c -O2 -o  ../exe/sos_triangle -lm "
      cc sos_triangle.c -O2 -o  ../exe/sos_triangle -lm 
