#!/bin/bash

SVN_USER=$USER
SWATYEAR=2011

# SVN_SSH="$ProgramFiles$//TortoiseSVN//bin//TortoisePlink.exe"
echo SVN_USER = $SVN_USER
echo SVN_SSH = $SVN_SSH

EXEC_CMD="svn co svn+ssh://$SVN_USER@faui6svn.informatik.uni-erlangen.de/proj/svn/lehre/swat/swat$SWATYEAR/junction/swat-default $HOME/svnlocal/swat-default"
echo "$EXEC_CMD"
$EXEC_CMD
