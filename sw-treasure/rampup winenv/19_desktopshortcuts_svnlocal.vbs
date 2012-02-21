set WshShell = WScript.CreateObject("WScript.Shell" ) 
strDesktop = WshShell.SpecialFolders("Desktop" )

set oShellLink = WshShell.CreateShortcut(strDesktop & "\swat-default.lnk" ) 
oShellLink.TargetPath = "%USERPROFILE%\svnlocal\swat-default" 
oShellLink.WindowStyle = 1 
oShellLink.Description = "svnlocal swat-default" 
oShellLink.WorkingDirectory = "%USERPROFILE%\svnlocal\swat-default" 
oShellLink.Save

set oShellLink = WshShell.CreateShortcut(strDesktop & "\sys-src.lnk" ) 
oShellLink.TargetPath = "%USERPROFILE%\svnlocal\swat-default\devT\sys-src"
oShellLink.WindowStyle = 1 
oShellLink.Description = "sys-src" 
oShellLink.WorkingDirectory = "%USERPROFILE%\svnlocal\swat-default\devT\sys-src" 
oShellLink.Save
