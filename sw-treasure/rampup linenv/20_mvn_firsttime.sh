#!/bin/bash
SWATDEVROOTDIR=$HOME/svnlocal/swat-default/devT/sys-src
cd $SWATDEVROOTDIR
mvn clean eclipse:clean -Dsilent=true
mvn install -Dsilent=true
mvn site:site -Dsilent=true
mvn dependency:go-offline -Dsilent=true
mvn dependency:sources -Dsilent=true
