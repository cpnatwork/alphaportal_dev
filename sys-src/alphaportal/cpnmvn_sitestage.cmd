@echo off

set PROJROOT=%CD%
set PROJHOME=%PROJROOT%

set MVNSTDOPT=-Dsilent=true -Dmaven.test.skip=false

cd /D %PROJHOME%
cmd /D/C mvn install %MVNSTDOPT%
cmd /D/C mvn jxr:jxr jxr:test-jxr %MVNSTDOPT%
rem cmd /D/C mvn site:site %MVNSTDOPT%
cmd /D/C mvn site:stage %MVNSTDOPT%
cmd /D/C mvn javancss:report %MVNSTDOPT%

pause