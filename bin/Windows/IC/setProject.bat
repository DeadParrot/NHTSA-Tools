@echo off
rem NHTSA Windows Intel Setup

if not (%NHTSA%) == () goto Step2
set NHTSA=%~dp0
set NHTSA=%NHTSA:~0,-16%
:Step2

set PlatformOS=Windows
set PlatformCompiler=Windows\IC

set Path=%Path%;%NHTSA%\bin\%PlatformCompiler%

set INCLUDE=.;%NHTSA%\src\inc\Windows\IC;%NHTSA%\src\inc\Windows;%NHTSA%\src\inc;%INCLUDE%

%~dp0..\setProject.bat
