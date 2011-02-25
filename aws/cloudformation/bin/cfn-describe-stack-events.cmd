@echo off

setlocal

REM Set intermediate env vars because the %VAR:x=y% notation below
REM (which replaces the string x with the string y in VAR)
REM doesn't handle undefined environment variables. This way
REM we're always dealing with defined variables in those tests.
set CHK_HOME=_%AWS_CLOUDFORMATION_HOME%

if "%CHK_HOME:"=%" == "_" goto HOME_MISSING

"%AWS_CLOUDFORMATION_HOME:"=%\bin\cfn-cmd.cmd" cfn-describe-stack-events %*
goto DONE
:HOME_MISSING
echo AWS_CLOUDFORMATION_HOME is not set
exit /b 1

:DONE
