@echo off
SETLOCAL EnableDelayedExpansion

rem --- WE WILL ALWAYS NEED A JDK ---
set MYJAVA_ROOT=C:\Programme\Java\jdk1.6.0_24
setx JAVA_ROOT "!MYJAVA_ROOT!"
set MYJAVA_HOME=!MYJAVA_ROOT!
setx JAVA_HOME "!MYJAVA_HOME!"
set MYJRE_HOME=!MYJAVA_HOME!\jre
setx JRE_HOME "!MYJRE_HOME!"
set MYJAVA_BINDIR=!MYJAVA_HOME!\bin
setx JAVA_BINDIR "!MYJAVA_BINDIR!"

set MYPATH=!MYJAVA_BINDIR!
set MYPATH=!MYPATH!;%USERPROFILE%\progs\eclipse
set MYPATH=!MYPATH!;%USERPROFILE%\progs\apache-maven-3.0.2\bin
setx PATH "!MYPATH!"

setx M2_HOME ""
setx M3_HOME "%USERPROFILE%\progs\apache-maven-3.0.2"

rem The following options reduce PermGem Exceptions:
set MYMAVEN_OPTS=-XX:PermSize=256m -XX:MaxPermSize=512m -XX:+CMSClassUnloadingEnabled -XX:+ExplicitGCInvokesConcurrentAndUnloadsClasses
setx MAVEN_OPTS "%MYMAVEN_OPTS%"

pause