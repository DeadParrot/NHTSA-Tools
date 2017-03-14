@echo off
rem NHTSA Windows Intel 64-Bit Setup

if not (%NHTSA%) == () goto Step2
set NHTSA=%~dp0
set NHTSA=%NHTSA:~0,-19%
:Step2

set PlatformBits=Windows\IC\64

set Path=%Path%;%NHTSA%\bin\%PlatformBits%

call setPython.64.bat
call setIC.64.bat

%~dp0..\setProject.bat
