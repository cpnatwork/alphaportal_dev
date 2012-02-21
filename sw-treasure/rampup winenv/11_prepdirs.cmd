@echo off
xcopy /S /E /C /Y dirs2copy\* "%USERPROFILE%"
mkdir "%USERPROFILE%\svnlocal"
mkdir "%USERPROFILE%\wsp.eclipse"
pause