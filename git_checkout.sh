#!/bin/sh

if [ $# -ne 1 ]; then
  echo "[ERROR] branch not specified"
  echo "Usage: ./git_checkout.sh BRANCH"
  exit 1
fi

for dir in `find deps/leo_* -maxdepth 0` deps/savannah_mq
do
  cd $dir
  echo $dir
  git checkout $1
  cd ../../
done
