#!/bin/ksh 
echo "cc sph2vrml.c -o ../exe/sph2vrm # sph2vrml is in c"
cc sph2vrml.c -o ../exe/sph2vrml # sph2vrml is in c
