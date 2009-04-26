echo off
cls
"%JAVA_HOME%/bin/java" -classpath bin com.beepell.deployment.transform.Transform %1 %2
