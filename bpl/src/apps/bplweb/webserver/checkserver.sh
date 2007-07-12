#! /bin/sh
# Check whether the server is running, restarting it if needed.

command='[r]uby script/server'

if ps -f x | grep "$command" > /dev/null
then
  true
else
  echo Restarting BPLweb web server
  ./start.sh > /dev/null 2>&1 &
fi

