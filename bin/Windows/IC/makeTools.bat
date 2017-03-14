@echo off
rem ****************************************************************************
rem Makes All NHTSA Libraries and Programs
rem
rem Language: Windows Batch
rem
rem Compiler: Intel Fortran
rem
rem Author: Stuart G. Mentzer
rem
rem Date: 2002/06/14
rem ****************************************************************************

if (%NHTSA%) == () goto NoVar

cd %NHTSA%\src\lib\Windows\IC\o
call mak
call mak install
call mak clean
cd %NHTSA%\src\Windows\IC\o
call mak
call mak clean
cd %NHTSA%\src\Plot\Windows\IC\o
call mak
call mak clean
cd %NHTSA%
goto Exit

:NoVar
 echo *** ERROR - NHTSA environment variable not set

:Exit
