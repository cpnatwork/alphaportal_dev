@echo off

set TUTHOME=%CD%\tutorialportal

cd /D %TUTHOME%\web
cmd /D/C mvn jetty:run-war

pause