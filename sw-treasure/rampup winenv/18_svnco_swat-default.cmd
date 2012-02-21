@echo off

set SVN_USER=%USERNAME%
set SVN_SSH="%ProgramFiles%\\TortoiseSVN\\bin\\TortoisePlink.exe"
echo SVN_USER = %SVN_USER%
echo SVN_SSH = %SVN_SSH%

set SWATYEAR=2011

set EXEC_CMD=svn co svn+ssh://%SVN_USER%@faui6svn.informatik.uni-erlangen.de:/proj/svn/lehre/swat/swat%SWATYEAR%/junction/swat-default "%USERPROFILE%\svnlocal\swat-default"
echo %EXEC_CMD%
cmd /D/C %EXEC_CMD%

pause