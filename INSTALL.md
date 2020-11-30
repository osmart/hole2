Installation of the open source version of HOLE
===============================================

Pre-built binaries are available for some operating systems from
http://www.holeprogram.org/; if you want to build from source then
follow the instructions below.


Compiling from source
---------------------

If your Fortran compiler is `gfortran` then all the defaults should
work for you and you can say

     cd src
     source ../source.apache
     make


If you have another Fortran compiler then you need to *always set the
`FC` variable when you invoke `make`*. In the example below I use
`FC=gfortran-mp-6` to show how to set it. 

    cd src
    source ../source.apache
    make FC=gfortran-mp-6
	

(Note: By sourcing `source.apache` the version of the binaries will
be set to whatever is in `source.apache`'s `HoleVersion` variable. If
you modify the sources then you should consider *not* setting these
environment variables.)

By default, `hole` and other binaries are put in the `exe` directory
at the root of the distribution.


Installation of standard executables
------------------------------------

If you want to install the **standard HOLE executables** (`hole`,
`sph_process`, `sos_triangle`, `qpt_conv`) and data files in a
*different location* than the `exe` directory then set the `PREFIX`
Makefile variable. For example, to install in `$HOME/hole2` run

    cd src
    make PREFIX=$HOME/hole2 install 

(Executables will be by default in `BIN_DIR=$PREFIX/bin` and data in
`DATA_DIR=$PREFIX/share/hole2`.)

The core executables are the ones used in the [HOLE: Example
Application](http://www.holeprogram.org/doc/index.html#_example_application)
in the official docs.

If you used a custom FORTRAN compiler, specify it again

    cd src
    make PREFIX=$HOME/hole2 FC=gfortran-mp-6 install 


Installation of other executables
---------------------------------

The hole distribution contains other executables than can be installed
separately:

For the **optional programs** (`labqpt`, `qplot`,
`vdwdot`, `vmd_triangles_to_lines`):

    cd src
    make PREFIX=$HOME/hole2 install-opt 

For the **2D mapping programs** (`make_post_map`, `bln2gnu`,
`make_post2gnu`, `capost2gnu`, `grd2gnu`):

    cd src
    make PREFIX=$HOME/hole2 install-2map

