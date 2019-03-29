@echo off
rem Intel Compiler 64-Bit Setup
rem Put a custom version first in your PATH to adapt to your system

rem Composer XE 2019
if not "%IFORT_COMPILER19%" == "" "%IFORT_COMPILER19%\bin\compilervars.bat" intel64

rem Composer XE 2016
if not "%IFORT_COMPILER16%" == "" "%IFORT_COMPILER16%\bin\compilervars.bat" intel64

rem Composer XE 2015
if not "%IFORT_COMPILER15%" == "" "%IFORT_COMPILER15%\bin\compilervars.bat" intel64

rem Composer XE 2013 SP1
if not "%IFORT_COMPILER14%" == "" "%IFORT_COMPILER14%\bin\compilervars.bat" intel64

rem Composer XE 2013
if not "%IFORT_COMPILER13%" == "" "%IFORT_COMPILER13%\bin\compilervars.bat" intel64

rem Composer XE 2011
if not "%IFORT_COMPILER12%" == "" "%IFORT_COMPILER12%\bin\compilervars.bat" intel64

rem Fortran 11.x
if not "%IFORT_COMPILER11%" == "" "%IFORT_COMPILER11%\bin\ifortvars.bat" intel64
