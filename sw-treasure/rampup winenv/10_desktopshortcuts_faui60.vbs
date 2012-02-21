set WshShell = WScript.CreateObject("WScript.Shell" ) 
strDesktop = WshShell.SpecialFolders("Desktop" )

set oShellLink = WshShell.CreateShortcut(strDesktop & "\faui60 - promed.lnk" ) 
oShellLink.TargetPath = "\\FAUI60\praktika\swat" 
oShellLink.WindowStyle = 1 
oShellLink.Description = "svnlocal alphaflow-default" 
oShellLink.WorkingDirectory = "\\FAUI60\praktika\swat" 
oShellLink.Save
