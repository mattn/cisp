#!/bin/sh

cd `dirname $0`
FILTER=.

if [ "x$1" != "x" ]; then
  FILTER=$1
fi

export CISP_BASE_DIR=..

/bin/ls *.lisp | grep $FILTER | /usr/bin/sort | while read file; do
  echo -n "$file: "
  if [ -e `basename $file .lisp`.skip ]; then
    echo "SKIP"
    continue
  fi
  if [ -e `basename $file .lisp`.out ]; then
    EXPECT=$(cat `basename $file .lisp`.out | tr -d "\r")
    ACTUAL=$(../cisp $file | tr -d "\r")
  else
    EXPECT=$(cat `basename $file .lisp`.err | tr -d "\r")
    ACTUAL=$(../cisp $file 2>&1 | tr -d "\r")
  fi
  if [ "$ACTUAL" != "$EXPECT" ]; then
    echo "NG"
    echo "EXPECT:"
    echo "$EXPECT" | nl
    echo "ACTUAL:"
    echo "$ACTUAL" | nl
    exit 1
  else
    echo "OK"
  fi
done
