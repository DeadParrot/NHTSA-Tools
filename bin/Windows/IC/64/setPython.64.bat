@echo off
:: Python 64-Bit Development Setup
:: Put a custom version earlier in your PATH to adapt to your system

if exist C:\Python310\python.exe (
  set PYTHON_DIR=C:\Python310
  set PYTHON_LIB=%PYTHON_DIR%\libs\python310.lib
) else (
if exist C:\Python\libs\python310.lib (
  set PYTHON_DIR=C:\Python
  set PYTHON_LIB=%PYTHON_DIR%\libs\python310.lib
) else (
if exist C:\Python39\python.exe (
  set PYTHON_DIR=C:\Python39
  set PYTHON_LIB=%PYTHON_DIR%\libs\python39.lib
) else (
if exist C:\Python\libs\python39.lib (
  set PYTHON_DIR=C:\Python
  set PYTHON_LIB=%PYTHON_DIR%\libs\python39.lib
) else (
if exist C:\Python38\python.exe (
  set PYTHON_DIR=C:\Python38
  set PYTHON_LIB=%PYTHON_DIR%\libs\python38.lib
) else (
if exist C:\Python\libs\python38.lib (
  set PYTHON_DIR=C:\Python
  set PYTHON_LIB=%PYTHON_DIR%\libs\python38.lib
) else (
if exist C:\Python37\python.exe (
  set PYTHON_DIR=C:\Python37
  set PYTHON_LIB=%PYTHON_DIR%\libs\python37.lib
) else (
if exist C:\Python\libs\python37.lib (
  set PYTHON_DIR=C:\Python
  set PYTHON_LIB=%PYTHON_DIR%\libs\python37.lib
) else (
if exist C:\Python36\python.exe (
  set PYTHON_DIR=C:\Python36
  set PYTHON_LIB=%PYTHON_DIR%\libs\python36.lib
) else (
if exist C:\Python\libs\python36.lib (
  set PYTHON_DIR=C:\Python
  set PYTHON_LIB=%PYTHON_DIR%\libs\python36.lib
) else (
if exist C:\Python27\python.exe (
  set PYTHON_DIR=C:\Python27
  set PYTHON_LIB=%PYTHON_DIR%\libs\python27.lib
) else (
if exist C:\Python\libs\python27.lib (
  set PYTHON_DIR=C:\Python
  set PYTHON_LIB=%PYTHON_DIR%\libs\python27.lib
) else (
  set PYTHON_DIR=
  echo Python not found: Put a custom setPython.64.bat file in your PATH
  exit /B 1
))))))))))))

set PYTHON_INC=%PYTHON_DIR%\include
set PYTHON_LIB_DIR=%PYTHON_DIR%\libs
set PYTHON_NUMPY_INC=%PYTHON_DIR%\lib\site-packages\numpy\core\include
set PYTHONDONTWRITEBYTECODE=x

echo. "%PATH%" | findstr /C:"%PYTHON_DIR%;%PYTHON_DIR%\Scripts;%PYTHON_DIR%\Tools\scripts" >nul 2>nul
if errorlevel 1 set PATH=%PATH%;%PYTHON_DIR%;%PYTHON_DIR%\Scripts;%PYTHON_DIR%\Tools\scripts
