@echo off
rem Copyright 2013 Google Inc. All Rights Reserved.

SET INTERACTIVE=1
echo %CmdCmdLine% | find /i "%~0" >nul
if not errorlevel 1 set INTERACTIVE=0

echo Welcome to the Google Cloud SDK!

SETLOCAL

IF "%CLOUDSDK_PYTHON%"=="" (
  for %%i in (python.exe) do (set FOUND_PYTHON=%%~$PATH:i)
  if NOT defined FOUND_PYTHON (
    echo.
    echo To use the Google Cloud SDK, you must have Python installed and on your PATH.
    echo As an alternative, you may also set the CLOUDSDK_PYTHON environment variable
    echo to the location of your Python executable.
    EXIT /B 1
  )
  SET CLOUDSDK_PYTHON=python.exe
)

cmd /c "%CLOUDSDK_PYTHON% "%~dp0bin\bootstrapping\install.py" %*"
if _%INTERACTIVE%_==_0_ (
  if _%CLOUDSDK_CORE_DISABLE_PROMPTS%_==__ (
    echo Google Cloud SDK installer will now exit.
    PAUSE
  )
)
IF %ERRORLEVEL% NEQ 0 (
  EXIT /B %ERRORLEVEL%
)

ENDLOCAL
