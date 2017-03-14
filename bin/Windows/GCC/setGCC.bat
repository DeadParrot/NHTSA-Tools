@echo off
rem GCC Compiler Setup

rem Put a custom version first in your PATH to adapt to your system

set MinGW=C:\MinGW
rem set Path=%MinGW%\bin;%Path%
set CPATH=%CPATH%;%MinGW%\opt\include
