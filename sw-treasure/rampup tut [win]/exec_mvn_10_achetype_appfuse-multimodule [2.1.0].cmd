@echo off

cmd /D/C mvn archetype:generate -B -DarchetypeGroupId=org.appfuse.archetypes -DarchetypeArtifactId=appfuse-modular-struts-archetype -DarchetypeVersion=2.1.0 -DgroupId=swat2011team -DartifactId=tutorialportal -DarchetypeRepository=http://oss.sonatype.org/content/repositories/appfuse

pause