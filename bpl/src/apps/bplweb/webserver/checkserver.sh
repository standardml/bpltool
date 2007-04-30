#! /bin/sh
# Check whether the server is running, restarting it if needed.

command='[r]uby script/server'

if ps -f x | grep "$command" > /dev/null
then
  true
else
  ./start.sh &
fi

