@echo off

set PROJROOT=%CD%
set PROJHOME=%PROJROOT%
set WEBHOME=%PROJHOME%/web

set MVNSTDOPT=-Dsilent=true -Dmaven.test.skip=false

cd /D %WEBHOME%
cmd /D/C mvn jetty:run %MVNSTDOPT%
pause