@echo off
rem NHTSA Windows Intel 64-Bit Release Setup

set NHTSA_bin=%~dp0
set NHTSA_bin=%NHTSA_bin:~0,-1%
if (%NHTSA%) == () set NHTSA=%NHTSA_bin:~0,-20%

set PlatformFull=Windows\IC\64\r

set Path=%Path%;%NHTSA_bin%

set LIB=%LIB%;%NHTSA_bin%

%~dp0..\setProject.bat
