#! /bin/sh
# Check whether the server is running, restarting it if needed.

command='[r]uby bplxmlrpcserver.rb'

if ps -f x | grep "$command" > /dev/null
then
  true
else
  echo Restarting XMLRPC server
  ./start.sh > /dev/null 2>&1 &
fi

