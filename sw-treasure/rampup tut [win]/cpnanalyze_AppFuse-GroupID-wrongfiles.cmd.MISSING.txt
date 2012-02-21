#!/bin/bash

GROUPID=swat2011team
ARTIFACTID=alphaportal
PACKAGE=alpha.portal

# because there is a 'bug' in AppFuse Archetype creation: initially set groupId to the intended package-name
mvn archetype:generate -B -DarchetypeGroupId=org.appfuse.archetypes -DarchetypeArtifactId=appfuse-modular-spring-archetype -DarchetypeVersion=2.1.0 -DarchetypeRepository=http://oss.sonatype.org/content/repositories/appfuse -DgroupId=${GROUPID} -DartifactId=${ARTIFACTID} -Dpackage=${PACKAGE}

grep -ir ${GROUPID} ${ARTIFACTID} | grep .xml:
