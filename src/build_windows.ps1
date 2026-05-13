# =====================================================
#  HOLE2 Windows Build Script (PowerShell + MinGW-w64)
#  Compiles HOLE2 using gfortran/gcc from MinGW-w64
#  Run from the src directory
# =====================================================

$ErrorActionPreference = "Continue"

# --- Configuration ---
$env:PATH = "C:\msys64\mingw64\bin;$env:PATH"
$FC = "gfortran"
$CC = "gcc"
$AR = "ar"
$RANLIB = "ranlib"
$FFLAGS = @("-fd-lines-as-comments", "-fbackslash", "-std=legacy", "-static-libgfortran", "-O2")
$CFLAGS = @("-O2")
$LFLAGS = @("-static")
$EXE_DIR = "..\exe"
$LIB_NAME = "hole.a"

Write-Host "==================================================="
Write-Host " HOLE2 Windows Build (MinGW-w64 gfortran)"
$ver = & $FC --version 2>&1 | Select-Object -First 1
Write-Host " Using: $ver"
Write-Host "==================================================="
Write-Host ""

# --- Create exe directory if needed ---
if (!(Test-Path $EXE_DIR)) { New-Item -ItemType Directory -Path $EXE_DIR | Out-Null }
if (!(Test-Path "$EXE_DIR\2dmap")) { New-Item -ItemType Directory -Path "$EXE_DIR\2dmap" | Out-Null }

# --- Step 1: Copy machine_dep for Windows ---
Write-Host "[1/6] Setting up machine_dep.f for Windows..."
Copy-Item -Force "machine_dep.win64" "machine_dep.f"

# --- Step 2: Generate vertim.f (version info) ---
Write-Host "[2/6] Generating vertim.f (version info)..."
@"
      SUBROUTINE VERTIM( RESOUT)
      IMPLICIT NONE
C s/r which gives the date of linking written for Windows build
      INTEGER RESOUT
      WRITE(RESOUT, '(A)') 
     &' ',
     &' For help on HOLE suite see'//
     &'  http://www.holeprogram.org/',
     &' ',
     &' HOLE release 2.3.1 (Windows Build 2026) ',
     &' ',
     &' usage subject to Apache License, Version 2.0 ',
     &' see http://www.apache.org/licenses/LICENSE-2.0 '
      RETURN
      END
"@ | Set-Content -Path "vertim.f" -Encoding ASCII

# --- Step 3: Compile all Fortran source files ---
Write-Host "[3/6] Compiling Fortran sources..."
$FORTRAN_FILES = @(
    "addend", "calper", "cguess", "cirova", "coarea", "concal", "freda",
    "get_rec_commands", "h2dmap", "hcapen", "hcapgr", "helefi", "hocapd",
    "hocapr", "hodotb", "hodotc", "hodotu", "hograp", "holcal", "holeen",
    "hole", "hollin", "holset", "homulf", "honewp", "honewv", "hopegg",
    "horadr", "horchr", "hosetg", "hsbxen", "hsbxmi", "hsurfp", "hydasc",
    "labqpt", "lastf", "linter", "lpause", "machine_dep", "make_pmap",
    "newop", "peg_writeall_header", "ptgen", "qgetbi", "qhsp", "qplot",
    "qpspic", "qpswr", "qpt_conv", "qptdot", "qptins", "qptkin", "qpto",
    "qptras", "qptspl", "qptsyb", "qptvmd", "qptvrm", "qreadi", "qusemx",
    "ratcal", "raten", "ratesc", "rcontr", "readse", "sbdclo", "sbgen",
    "sbgrow", "sphpeg", "sph_process", "sph_process_read", "sphqpc",
    "sphqpu", "sphtri", "ssafn2", "textat", "trisphere", "tsatr", "tsradr",
    "ut_strings", "ut_vector", "vdrcon", "vdset", "vdwdot",
    "vmd_triangles_to_lines", "whatu", "wmolqp", "wpdbsp", "vertim"
)

$failCount = 0
$successCount = 0
foreach ($f in $FORTRAN_FILES) {
    $src = "${f}.f"
    $obj = "${f}.o"
    $output = & $FC $FFLAGS -c $src -o $obj 2>&1
    if ($LASTEXITCODE -ne 0) {
        Write-Host "  FAILED: ${src}" -ForegroundColor Red
        if ($output) { Write-Host "    $output" -ForegroundColor Yellow }
        $failCount++
    } else {
        $successCount++
    }
}
Write-Host "  Compiled: $successCount OK, $failCount FAILED (out of $($FORTRAN_FILES.Count))"

if ($failCount -gt 0) {
    Write-Host ""
    Write-Host "WARNING: $failCount files failed to compile!" -ForegroundColor Yellow
    Write-Host ""
}

# --- Step 4: Compile C sources ---
Write-Host "[4/6] Compiling C sources..."
$output = & $CC $CFLAGS -c hp_flush.c -o hp_flush.o 2>&1
if ($LASTEXITCODE -ne 0) {
    Write-Host "  FAILED: hp_flush.c" -ForegroundColor Red
    if ($output) { Write-Host "    $output" -ForegroundColor Yellow }
} else {
    Write-Host "  OK: hp_flush.c"
}

# --- Step 5: Create static library ---
Write-Host "[5/6] Creating static library ${LIB_NAME}..."
if (Test-Path $LIB_NAME) { Remove-Item -Force $LIB_NAME }

$objFiles = @()
foreach ($f in $FORTRAN_FILES) {
    if (Test-Path "${f}.o") { $objFiles += "${f}.o" }
}
if (Test-Path "hp_flush.o") { $objFiles += "hp_flush.o" }

& $AR rcv $LIB_NAME $objFiles 2>&1 | Out-Null
& $RANLIB $LIB_NAME 2>&1 | Out-Null
Write-Host "  Created ${LIB_NAME} ($($objFiles.Count) objects)"

# --- Step 6: Link executables ---
Write-Host "[6/6] Linking executables..."

function Link-Program {
    param([string]$Name, [string]$ObjFile, [string]$OutputDir = $EXE_DIR, [switch]$IsC)
    
    $outPath = "$OutputDir\${Name}.exe"
    if ($IsC) {
        $output = & $CC $CFLAGS $LFLAGS sos_triangle.c -O2 -o $outPath -lm 2>&1
    } else {
        $output = & $FC $FFLAGS $LFLAGS $ObjFile $LIB_NAME -o $outPath 2>&1
    }
    if ($LASTEXITCODE -ne 0) {
        Write-Host "  FAILED: ${Name}.exe" -ForegroundColor Red
        if ($output) { Write-Host "    $output" -ForegroundColor Yellow }
        return $false
    } else {
        $size = (Get-Item $outPath).Length / 1MB
        Write-Host ("  OK: ${Name}.exe ({0:N1} MB)" -f $size)
        return $true
    }
}

# Core programs
Write-Host "  --- Core Programs ---"
Link-Program "hole" "hole.o"
Link-Program "sph_process" "sph_process.o"
Link-Program "qpt_conv" "qpt_conv.o"
Link-Program "sos_triangle" "" -IsC

# Optional programs
Write-Host "  --- Optional Programs ---"
Link-Program "labqpt" "labqpt.o"
Link-Program "qplot" "qplot.o"
Link-Program "vdwdot" "vdwdot.o"
Link-Program "vmd_triangles_to_lines" "vmd_triangles_to_lines.o"

# 2D map programs
Write-Host "  --- 2D Map Programs ---"
Link-Program "make_post_map" "make_pmap.o" "$EXE_DIR\2dmap"

foreach ($prog in @("bln2gnu", "make_post2gnu", "capost2gnu", "grd2gnu")) {
    $output = & $FC $FFLAGS $LFLAGS "2dmap_with_gnuplot\${prog}.f" $LIB_NAME -o "$EXE_DIR\2dmap\${prog}.exe" 2>&1
    if ($LASTEXITCODE -ne 0) {
        Write-Host "  FAILED: ${prog}.exe" -ForegroundColor Red
        if ($output) { Write-Host "    $output" -ForegroundColor Yellow }
    } else {
        $size = (Get-Item "$EXE_DIR\2dmap\${prog}.exe").Length / 1MB
        Write-Host ("  OK: ${prog}.exe ({0:N1} MB)" -f $size)
    }
}

Write-Host ""
Write-Host "==================================================="
Write-Host " Build complete!"
Write-Host " Executables are in: $EXE_DIR"
Write-Host "==================================================="
Write-Host ""
Write-Host "Built executables:"
Get-ChildItem "$EXE_DIR\*.exe" 2>$null | ForEach-Object { Write-Host ("  {0} ({1:N1} MB)" -f $_.Name, ($_.Length / 1MB)) }
Write-Host ""
Write-Host "2D map executables:"
Get-ChildItem "$EXE_DIR\2dmap\*.exe" 2>$null | ForEach-Object { Write-Host ("  {0} ({1:N1} MB)" -f $_.Name, ($_.Length / 1MB)) }
