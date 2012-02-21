#!/bin/bash

TUTHOME=`pwd`/tutorialportal

cd $TUTHOME/web
mvn jetty:run-war
