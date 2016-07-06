#!/bin/sh

cd `dirname $0`

/bin/ls *.lisp | /usr/bin/sort | while read file; do
  echo -n "$file: "
  EXPECT=$(cat `basename $file .lisp`.out)
  ACTUAL=$(../cisp $file | tr -d "\r")
  if [ "$ACTUAL" != "$EXPECT" ]; then
    echo "NG"
    echo "EXPECT: $EXPECT"
    echo "ACTUAL: $ACTUAL"
    exit 1
  else
    echo "OK"
  fi
done
