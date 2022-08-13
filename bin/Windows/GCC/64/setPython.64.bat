@echo off
:: Python 64-Bit Development Setup
:: Put a custom version earlier in your PATH to adapt to your system

for %%V in (315 314 313 312 311 310 39 38 37 36 35 27) do (
  if exist C:\Python%%V\python.exe (
    set PYTHON_DIR=C:\Python%%V
    set PYTHON_VER=%%V
    goto Step2
  ) else (
  if exist C:\Python\libs\python%%V.lib (
    set PYTHON_DIR=C:\Python
    set PYTHON_VER=%%V
    goto Step2
  ) else (
  if exist "C:\Program Files\Python%%V\python.exe" (
    set "PYTHON_DIR=C:\Program Files\Python%%V"
    set PYTHON_VER=%%V
    goto Step2
  ) else (
  if exist "C:\Program Files\Python\libs\python%%V.lib" (
    set "PYTHON_DIR=C:\Program Files\Python"
    set PYTHON_VER=%%V
    goto Step2
  ) else (
  if exist "C:%HOMEPATH%\AppData\Local\Programs\Python\Python%%V\python.exe" (
    set "PYTHON_DIR=C:%HOMEPATH%\AppData\Local\Programs\Python\Python%%V"
    set PYTHON_VER=%%V
    goto Step2
  )))))
)
echo Python not found
exit /B 1

:Step2
set "PYTHON_INC=%PYTHON_DIR%\include"
set "PYTHON_LIB_DIR=%PYTHON_DIR%\libs"
set "PYTHON_LIB=%PYTHON_LIB_DIR%\python%PYTHON_VER%.lib"
set "PYTHON_NUMPY_INC=%PYTHON_DIR%\lib\site-packages\numpy\core\include"
set PY_PYTHON=%PYTHON_VER:~0,1%.%PYTHON_VER:~1%
set PYTHONDONTWRITEBYTECODE=x

echo. "%PATH%" | findstr /C:"%PYTHON_DIR%;%PYTHON_DIR%\Scripts;%PYTHON_DIR%\Tools\scripts" >nul 2>nul
if errorlevel 1 set "PATH=%PATH%;%PYTHON_DIR%;%PYTHON_DIR%\Scripts;%PYTHON_DIR%\Tools\scripts"
