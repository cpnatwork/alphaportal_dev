@echo off

set TUTHOME=%CD%\tutorialportal

cd /D %TUTHOME%
cmd /D/C mvn clean eclipse:clean -Dsilent=true
cmd /D/C mvn install -Dsilent=true
cmd /D/C mvn dependency:go-offline -Dsilent=true
cmd /D/C mvn dependency:sources -Dsilent=true

cmd /D/C mvn site:site -Dsilent=true

pause