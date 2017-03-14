@echo off
rem NHTSA Windows Setup

if not (%NHTSA%) == () goto Step2
set NHTSA=%~dp0
set NHTSA=%NHTSA:~0,-13%
:Step2

set Path=%Path%;%NHTSA%\bin\Windows;%NHTSA%\bin
