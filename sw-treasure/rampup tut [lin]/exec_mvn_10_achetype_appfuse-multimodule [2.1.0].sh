#!/bin/bash

mvn archetype:generate -B -DarchetypeGroupId=org.appfuse.archetypes -DarchetypeArtifactId=appfuse-modular-spring-archetype -DarchetypeVersion=2.1.0 -DgroupId=swatteam -DartifactId=tutorialportal -Dpackagename=tutport -DarchetypeRepository=http://oss.sonatype.org/content/repositories/appfuse
