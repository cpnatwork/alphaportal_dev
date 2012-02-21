@echo off
SetLocal EnableDelayedExpansion

rem -- Nullsoft Scriptable Install System
rem *****************************************************
rem * /S Silent Mode
rem * /D=x:\dirname Installationspfad
rem * /NCRC Überspringt die CRC Prüfung
rem *****************************************************
set PARAM=/S
for %%F in (*.NSIS.exe) do (
	start /wait %%F !PARAM!
)

rem -- Wise Installer
rem *****************************************************
rem * /S Silent Mode
rem * /X Pathname extracts files into pathname
rem * /Z Pathname extracts files into pathname and reboots
rem * /M Prompts for windows, system, temp dirs
rem * /M=filename Specifies a value file, but for standard variables
rem *****************************************************
set PARAM=/s
for %%F in (*.WISE.exe) do (
	start /wait %%F !PARAM!
)

rem -- WinRAR SFX Archive
rem *****************************************************
rem * /s Silent Mode
rem *****************************************************
set PARAM=/s
for %%F in (*.WinRAR-SFX.exe) do (
	start /wait %%F !PARAM!
)

rem -- Inno Setup
rem *****************************************************
rem * /VERYSILENT Silent Mode
rem * /SILENT Silent Mode mit Statusbar
rem * /SP- Schaltet das "This will install... Do you wish to continue?" Fenster aus
rem * /SUPPRESSMSGBOXES Unterdrückt Nachrichtenfenster (funktioniert nur mit /(VERY)SILENT), wenn man eine Auswahl hat (z.B. überschreiben, abbrechen usw.)
rem * /NORESTART Unterdrückt notwendige Neustarts
rem * /LOADINF="filename" Instructs Setup to load the settings from the specified file after having checked the command line. This file can be prepared using the '/SAVEINF=' command as explained below. Don't forget to use quotes if the filename contains spaces.
rem * /SAVEINF="filename" Instructs Setup to save installation settings to the specified file. Don't forget to use quotes if the filename contains spaces.
rem * /LANG=language Spezifiziert die Sprache Specifies the language to use. language specifies the internal name of the language as specified in a [Languages] section entry.
rem * /DIR="x:\dirname" Installationsordner ändern
rem * /GROUP="folder name" Overrides the default folder name displayed on the Select Start Menu Folder wizard page. If the [Setup] section directive DisableProgramGroupPage was set to yes, this command line parameter is ignored.
rem * /NOICONS Instructs Setup to initially check the Don't create any icons check box on the Select Start Menu Folder wizard page.
rem * /COMPONENTS="comma separated list of component names" Overrides the default components settings. Using this command line parameter causes Setup to automatically select a custom type.
rem * /TASKS="comma separated list of task names" Specifies a list of tasks that should be initially selected or deselected. To deselect a task, prefix its name with a "!" character. Only the specified tasks (and their children) will be selected; the rest will be deselected. Use the /MERGETASKS parameter instead if you want to keep the default set of tasks and only select/deselect some of them.
rem * /MERGETASKS="comma separated list of task names" Like the /TASKS parameter, except the specified tasks will be merged with the set of tasks that would have otherwise been selected by default. If UsePreviousTasks is set to yes, the specified tasks will be selected/deselected after any previous tasks are restored.
rem *****************************************************
set PARAM=/sp- /norestart /silent
for %%F in (*.Inno.exe) do (
	start /wait %%F !PARAM!
)

rem -- Microsoft Windows Installer
rem *****************************************************
rem * /quiet Hintergrundmodus, keine Benutzerinteraktion
rem * /passive Silent Mode mit Statusleiste
rem * /qn keine Oberfläche
rem * /qb einfache Oberfläche
rem * /qr reduzierte Oberfläche
rem * INSTALLDIR= Installationspfad [wird nicht immer unterstützt!]
rem * TARGETDIR= Installationspfad "for an administrative Windows Installer based installation"
rem * /norestart kein Neustart nach der Installation, manchmal auch /noreboot oder Reboot=Supress
rem * /forcerestart Computer nach der Installation immer neu starten
rem *****************************************************
set PARAM=/quiet /passive /qb /norestart REBOOT=ReallySuppress
for %%F in (*.msi) do (
	start /wait msiexec /i %%F !PARAM!
)

rem -- InstallShield PFTW + MSI [requires NO setup.iss]
rem *****************************************************
rem * /a Add following parameters to the archived msi.
rem * /s Silent install *.exe packaged.
rem * /v Parameters used after this, are passed to the *.msi inside a package.
rem *****************************************************
set PARAM=/a /s /sms /v"/quiet /passive /qb /norestart REBOOT=ReallySuppress"
for %%F in (*.IS+MSI.exe) do (
	start /wait %%F !PARAM!
)

rem -- InstallShield - Package for the Web [requires: .\setup.iss]
rem *****************************************************
rem * /a Add following parameters to the archived setup.exe.
rem * /f1 Recorded file location.
rem * /p Installer Password.
rem * /sms Wait command, if second process is spawned.
rem * /s Silent install *.exe.
rem * /qn Silent install *.msi.
rem * /f2 Log file location.
rem *****************************************************
set PARAM=/s /a /s /sms
for %%F in (*.ISPFTW.exe) do (
	start /wait %%F !PARAM!
)

rem -- InstallShield - tratiditional [requires: .\setup.iss]
rem *****************************************************
rem * /a Add following parameters to the archived setup.exe.
rem * /f1 Recorded file location.
rem * /p Installer Password.
rem * /sms Wait command, if second process is spawned.
rem * /s Silent install *.exe.
rem * /qn Silent install *.msi.
rem * /f2 Log file location.
rem *****************************************************
set PARAM=/s /sms
for %%F in (*.IS.exe) do (
	start /wait %%F !PARAM!
)

rem -- IExpress
rem *****************************************************
rem * /Q Specifies quiet mode, or suppresses prompts, when files are being extracted.
rem * /Q:U Specifies user-quiet mode, which presents some dialog boxes to the user.
rem * /Q:A Specifies administrator-quiet mode, which does not present any dialog boxes to the user.
rem * /C: Specifies the path and name of the Setup .inf or .exe file.
rem * /R:N Never restarts the computer after installation.
rem * /R:A Always restarts the computer after installation.
rem * /R:S Restarts the computer after installation without prompting the user.
rem * /T: Specifies the target folder for extracting files.
rem *****************************************************
set PARAM=/Q /R:N
for %%F in (*.IExpress.exe) do (
	start /wait %%F !PARAM!
)

rem -- Tarma Installer
rem *****************************************************
rem * /q 
rem *****************************************************
set PARAM=/q
for %%F in (*.Tarma.exe) do (
	start /wait %%F !PARAM!
)
