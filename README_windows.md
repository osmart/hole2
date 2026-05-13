# HOLE2 — Windows 64-bit Build Guide

## Prerequisites

Install [MSYS2](https://www.msys2.org/) and then open an MSYS2 terminal to install gfortran:

```bash
pacman -Sy mingw-w64-x86_64-gcc-fortran
```

Or install MSYS2 automatically via winget:

```powershell
winget install -e --id MSYS2.MSYS2
# then open MSYS2 and run:
pacman -Sy mingw-w64-x86_64-gcc-fortran
```

## Building

From a PowerShell window, run the build script from the `src` directory:

```powershell
Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
cd src
.\build_windows.ps1
```

Executables are placed in the `exe\` directory. All 13 programs are built and
statically linked (no MinGW DLLs needed at runtime).

## Usage

HOLE reads its control file from standard input:

```powershell
Get-Content hole.inp | .\exe\hole.exe
```

Or in CMD:

```cmd
hole.exe < hole.inp
```

Example `hole.inp`:

```
coord  myprotein.pdb
radius C:\path\to\hole2\rad\simple.rad
sphpdb hole_out.sph
endrad 5.
```

> **Note:** Tilde paths (`~/hole2/rad/`) are not supported on Windows.
> Use full paths or relative paths instead.

## Using with mdahole2 (Python)

```python
from mdahole2.analysis import HoleAnalysis
ha = HoleAnalysis(u, executable=r"C:\path\to\hole2\exe\hole.exe")
ha.run()
```

## What was changed for Windows

| File | Change |
|---|---|
| `machine_dep.win64` | New — replaces Unix `date` shell call with `DATE_AND_TIME()` intrinsic; tilde expansion is a no-op |
| `build_windows.ps1` | New — PowerShell build script |
| `cguess.f` | Fixed REAL DO loop variable (`DO RCOUNT=-5.,5.,1.`) which crashes gfortran 16 on Windows |
| `hp_flush.c` | Added `flush_()` alias for gfortran name-mangling |

The build script copies `machine_dep.win64` → `machine_dep.f` before compiling,
so the original Linux/macOS build is completely unaffected.

## Tested

- Compiler: GNU Fortran 16.1.0 (MinGW-w64, MSYS2)
- OS: Windows 10/11 x64
- Verified with gramicidin example (1GRM): Rmin = 1.198 Å
