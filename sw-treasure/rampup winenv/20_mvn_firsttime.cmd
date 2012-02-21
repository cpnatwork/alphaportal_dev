@echo off
set SWATDEVROOTDIR=%USERPROFILE%\svnlocal\swat-default\devT\sys-src
cd /D %SWATDEVROOTDIR%
cmd /D/C mvn clean eclipse:clean -Dsilent=true
cmd /D/C mvn install -Dsilent=true
cmd /D/C mvn site:site -Dsilent=true
cmd /D/C mvn dependency:go-offline -Dsilent=true
cmd /D/C mvn dependency:sources -Dsilent=true
pause