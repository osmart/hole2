hole
====

A program to analyze the pore dimensions of ion channels. 

Please see the homepage http://www.holeprogram.org/ for further information.

Supported platforms
-------------------

* Linux
* macOS (experimental, Intel chips confirmed to work)

Installation
------------

* **Source code** is available at https://github.com/osmart/hole2/ . Compiling requires a working FORTRAN compiler. See [INSTALL.md](INSTALL.md) for installation instructions.

* For **Linux**, the [hole2 conda-forge package](https://anaconda.org/conda-forge/hole2) is available and can be installed with
  ```bash
  conda -c conda-forge install hole2
  ```
* For **macOS** with *Intel chips*  an _experimental_ conda-forge [hole2 package](https://anaconda.org/conda-forge/hole2) is also available. (The newer ARM64 (M1/M2) chips are currently not supported as a conda-forge package, see issue [#3](https://github.com/osmart/hole2/issues/3).)

* For **Windows** you need to compile `hole2` yourself but this has never been officially supported. If you succeed in building hole2 in Windows then please share successful recipes in issue [#16](https://github.com/osmart/hole2/issues/16).
