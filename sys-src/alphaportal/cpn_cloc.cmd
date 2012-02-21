@echo off

set EXEC=cloc-1.53.exe --exclude-dir=target,.svn,testrt,archive,_archive,.settings,agitar --force-lang=Java,drl

del *.lang.cloc
for /D %%D in (core web) do (
	cmd /D/C %EXEC% --report-file=%%D.lang.cloc %%D
)

set SUMREPORT=all
cmd /D/C cloc-1.53.exe --sum-reports --report_file=%SUMREPORT% *.lang.cloc
move %SUMREPORT%.lang %SUMREPORT%.lang.cloc
move %SUMREPORT%.file %SUMREPORT%.file.cloc

type %SUMREPORT%.file.cloc
type %SUMREPORT%.lang.cloc

pause