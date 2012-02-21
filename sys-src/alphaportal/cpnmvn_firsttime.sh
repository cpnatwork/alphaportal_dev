#!/bin/bash

PROJROOT=`pwd`
PROJHOME=$PROJROOT

cd $PROJROOT
mvn clean eclipse:clean -Dsilent=true
mvn dependency:go-offline -Dsilent=true
mvn dependency:sources -Dsilent=true
mvn install -Dsilent=true

cd $PROJHOME
mvn jxr:jxr jxr:test-jxr -Dsilent=true
mvn site:site -Dsilent=true
mvn javancss:report -Dsilent=true
