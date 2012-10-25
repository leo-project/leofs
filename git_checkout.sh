#!/bin/sh

for dir in `find deps/leo_* -maxdepth 0`
do
  cd $dir
  git checkout $1
  cd ../../
done
