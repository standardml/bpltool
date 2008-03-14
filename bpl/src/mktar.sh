#! /bin/sh

svn=svn

programname=`basename "$0"`
usage () {
  echo Usage: $programname NAME VERSION
  echo "  Create in the current directory a gzipped TAR file"
  echo "  NAME-VERSION.REVISION.tar.gz of the source files."
  echo "  Source files under SVN control are included, unless they"
  echo "  are listed in .tarexclude, which is local for each subdirectory."
  echo "  REVISION is the newest revision in the SVN repository."
  exit $1
}

[ -z "$2" ] && usage 1

case "$1" in
  -h | -help | --help) usage 0 ;;
esac

name="$1"
version="$2"
revision=`svn info . -r head | grep '^Revision:' | tr -c -d 0123456789`
tarbase="$name-$version.$revision"
tar="$tarbase.tar.gz"

if [ -e $tar ]
then
  echo Error: $tar exists
  exit 1
fi

echo Building directory tree for archive $tar...

mksoftlinks () {
  echo $PWD
  local down="$1"
  local up="$2"

  if [ -r .tarexclude ]
  then
  	greptarexclude="grep -v -F -f .tarexclude"
  else
    greptarexclude=cat
  fi
  files=`$svn list | grep -v -e / | $greptarexclude`
  dirs=`$svn list | grep / | $greptarexclude`
  for file in $files
  do
  	ln -s "$up../$down$file" "$up$tarbase/$down$file" || exit 1
  done
  for dir in $dirs
  do
    mkdir "$up$tarbase/$down$dir" || exit 1
  	cd "$dir" || exit 1
  	mksoftlinks "$down$dir" $up../
  	cd .. || exit 1
  done
}

mkdir $tarbase || exit 1
mksoftlinks "" ""
echo Creating archive $tar...
tar czhf $tar $tarbase || exit 1
rm -r $tarbase || exit 1