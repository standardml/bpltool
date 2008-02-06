#! /bin/sh

# Copyright (c) 2007  The BPL Group at the IT University of Copenhagen

# This file is part of BPL.

# BPL is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at
# your option) any later version.

# BPL is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with BPL; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
# USA


# Start the web server and listen to the restart pipe:
# whenever a line of arbitrary text is read from the pipe,
# restart the server.

if [ -r $HOME/share/rubygems/local/lib/site_ruby/1.8 ]
then
  export RUBYLIB=$HOME/share/rubygems/local/lib/site_ruby/1.8
fi
export RUBYOPT=-rrubygems
export GEM_HOME=$HOME/share/rubygems/gemrepo

if [ -e restart ]
then
  if [ ! -p restart ] 
  then
    echo "Error: file 'restart' should be a named pipe!"
    exit 1
  fi
else
  mkfifo -m ug=rw restart
fi

while true
do
  ruby script/server -p 8080 >> log/webserver.log 2>&1 &
  pid=$!
  read dummy < restart
  kill $pid
  wait $pid # Make sure zombie process is completely garbage collected
done
