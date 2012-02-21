@echo off

set PROJROOT=%CD%
set PROJHOME=%PROJROOT%

cd /D %PROJROOT%
cmd /D/C mvn clean eclipse:clean -Dsilent=true
cmd /D/C mvn dependency:go-offline -Dsilent=true
cmd /D/C mvn dependency:sources -Dsilent=true
cmd /D/C mvn install -Dsilent=true

cd /D %PROJHOME%
cmd /D/C mvn jxr:jxr jxr:test-jxr -Dsilent=true
cmd /D/C mvn site:site -Dsilent=true
cmd /D/C mvn javancss:report -Dsilent=true

pause