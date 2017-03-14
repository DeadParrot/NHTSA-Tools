@echo off
rem NHTSA Windows GCC 64-Bit Release Setup

set NHTSA_bin=%~dp0
set NHTSA_bin=%NHTSA_bin:~0,-1%
if (%NHTSA%) == () set NHTSA=%NHTSA_bin:~0,-21%

set PlatformFull=Windows\GCC\64\r

set Path=%Path%;%NHTSA_bin%

set LIBRARY_PATH=%NHTSA_bin%;%LIBRARY_PATH%

set LD_LIBRARY_PATH=%NHTSA_bin%;%LD_LIBRARY_PATH%

%~dp0..\setProject.bat
