#!/bin/bash

TUTHOME=`pwd`/tutorialportal

cd $TUTHOME
mvn clean eclipse:clean -Dsilent=true
mvn install -Dsilent=true
mvn dependency:go-offline -Dsilent=true
mvn dependency:sources -Dsilent=true

mvn site:site -Dsilent=true
