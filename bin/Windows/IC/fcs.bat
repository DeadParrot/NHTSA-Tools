@echo off
rem Intel Fortran Compile Syntax

if exist %1 (
  set FortranFile=%1
) else if exist %1.for (
  set FortranFile=%1.for
) else if exist %1.f90 (
  set FortranFile=%1.f90
) else if exist %1.f (
  set FortranFile=%1.f
) else (
  echo No Fortran source file matching name %1 found
  goto End
)

ifort /nologo /warn /nogen-interfaces /assume:nosource_include /noaltparam /syntax_only /c %FortranFile% %2 %3 %4 %5 %6 %7 %8 %9

set FortranFile=

:End
