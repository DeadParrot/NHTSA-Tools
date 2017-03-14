@echo off
rem GFortran Build Debug

if exist %1.for (
  set FortranFile=%1.for
) else if exist %1.f90 (
  set FortranFile=%1.f90
) else if exist %1.f (
  set FortranFile=%1.f
) else (
  echo No Fortran source file matching name %1 found
  goto End
)

gfortran -pipe -Wall -Wno-character-truncation -m64 -mtune=generic -fno-sign-zero -fno-automatic -ffree-line-length-0 -ffloat-store -fcheck=bounds -fbacktrace -ftrapv -finit-integer=-999999 -finit-real=nan -finit-character=33 -O0 -ggdb -Iinc -o %1.exe -x f95-cpp-input %FortranFile% %2 %3 %4 %5 %6 %7 %8 %9

set FortranFile=

:End
