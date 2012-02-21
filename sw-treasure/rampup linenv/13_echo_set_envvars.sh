#!/bin/bash

# --- WE WILL ALWAYS NEED A JDK ---
MYJAVA_ROOT=/usr/lib64/jvm/java-1.6.0
echo export JAVA_ROOT=\"$MYJAVA_ROOT\"
MYJAVA_HOME=$MYJAVA_ROOT
echo export JAVA_HOME=\"$MYJAVA_HOME\"
MYJRE_HOME=$MYJAVA_HOME/jre
echo export JRE_HOME=\"$MYJRE_HOME\"
MYJAVA_BINDIR=$MYJAVA_HOME/bin
echo export JAVA_BINDIR=\"$MYJAVA_BINDIR\"

MYPATH=$MYJAVA_BINDIR
MYPATH=$MYPATH:$HOME/progs/eclipse
MYPATH=$MYPATH:$HOME/progs/apache-maven-3.0.2/bin
echo export PATH=\"\$PATH:$MYPATH\"

echo export M2_HOME=\"\"
echo export M3_HOME=\"\$HOME/progs/apache-maven-3.0.2\"

# The following options reduce PermGem Exceptions:
MYMAVEN_OPTS="-XX:PermSize=256m -XX:MaxPermSize=512m -XX:+CMSClassUnloadingEnabled -XX:+ExplicitGCInvokesConcurrentAndUnloadsClasses"
echo export MAVEN_OPTS=\"$MYMAVEN_OPTS\"

