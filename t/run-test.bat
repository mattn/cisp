@echo off

set CWD=%~dp0
set CWD=%CWD:\=/%
bash %CWD%run-test.sh %*
rem bash t/
