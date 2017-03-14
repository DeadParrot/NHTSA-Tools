@echo off
rem NHTSA Windows GCC Setup

if not (%NHTSA%) == () goto Step2
set NHTSA=%~dp0
set NHTSA=%NHTSA:~0,-17%
:Step2

set PlatformOS=Windows
set PlatformCompiler=Windows\GCC

set Path=%Path%;%NHTSA%\bin\%PlatformCompiler%

call setGCC.bat

set CPATH=.;%NHTSA%\src\inc\Windows\GCC;%NHTSA%\src\inc\Windows;%NHTSA%\src\inc;%CPATH%

%~dp0..\setProject.bat
