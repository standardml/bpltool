#!/bin/sh
clear
$JAVA_HOME/bin/java -classpath lib/xsom.jar:lib/relaxngDatatype.jar:lib/epsgraphics-1.1.jar:lib/wsdl4j.jar:bin com.beepell.tools.launch.Launch $1
