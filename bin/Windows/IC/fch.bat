@echo off
rem Intel Fortran Compile Syntax Header

if exist %1 (
  set FortranHeader=%1
) else if exist %1.fh (
  set FortranHeader=%1.fh
) else if exist %1.fi (
  set FortranHeader=%1.fi
) else (
  echo No Fortran header file matching name %1 found
  goto End
)

if exist %FortranHeader%.for (
  echo *** ERROR: Temp file %FortranHeader%.for exists
  goto End
)

echo INCLUDE '%FortranHeader%' > %FortranHeader%.for
echo END >> %FortranHeader%.for
call fcs %FortranHeader%.for /warn:declarations %2 %3 %4 %5 %6 %7 %8 %9
del %FortranHeader%.for
goto End

:End
set FortranHeader=
