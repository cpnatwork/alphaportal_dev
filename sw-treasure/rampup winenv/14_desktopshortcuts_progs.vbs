set WshShell = WScript.CreateObject("WScript.Shell" ) 
strDesktop = WshShell.SpecialFolders("Desktop" )

set oShellLink = WshShell.CreateShortcut(strDesktop & "\user bin.lnk" ) 
oShellLink.TargetPath = "%USERPROFILE%\progs\bin\" 
oShellLink.WindowStyle = 1 
oShellLink.Description = "user progs/bin directory" 
oShellLink.WorkingDirectory = "%USERPROFILE%\progs\bin\" 
oShellLink.Save


set oShellLink = WshShell.CreateShortcut(strDesktop & "\pageant.lnk" ) 
oShellLink.TargetPath = "%USERPROFILE%\progs\bin\pageant.exe" 
oShellLink.WindowStyle = 1 
oShellLink.Description = "pageant" 
oShellLink.WorkingDirectory = "%USERPROFILE%\progs\bin\" 
oShellLink.Save

set oShellLink = WshShell.CreateShortcut(strDesktop & "\workspace eclipse (preped).lnk" )  
oShellLink.TargetPath = "%USERPROFILE%\wsp.eclipse\"
oShellLink.WindowStyle = 1 
oShellLink.Description = "Eclipse alpha-Flow" 
oShellLink.WorkingDirectory = "%USERPROFILE%\wsp.eclipse\" 
oShellLink.Save

set oShellLink = WshShell.CreateShortcut(strDesktop & "\eclipse - alphaflow.lnk" )  
oShellLink.TargetPath = "%USERPROFILE%\progs\eclipse\eclipse.exe"
oShellLink.WindowStyle = 1 
oShellLink.Description = "Eclipse alpha-Flow" 
oShellLink.WorkingDirectory = "%USERPROFILE%\progs\eclipse\" 
oShellLink.Save
