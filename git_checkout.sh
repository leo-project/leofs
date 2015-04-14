#!/bin/sh

usage()
{
  echo "Usage: ./git_checkout.sh [-h|--help] [--pull] BRANCH"
}

WITH_PULL=1
BRANCH=

for opt in $@; do
  case ${opt} in
    -h|--help)
      usage
      exit 1
      ;;
    --pull)
      WITH_PULL=0
      ;;
    *)
      BRANCH=${opt}
      ;;
  esac
  shift
done

if [ -z "${BRANCH}" ]; then
  echo "[ERROR] branch not specified"
  usage
  exit 1
fi

for dir in `find deps/leo_* -maxdepth 0` deps/savanna_commons deps/savanna_agent
do
  cd $dir
  echo $dir
  git checkout ${BRANCH}
  if [ ${WITH_PULL} = 0 ]; then
    git pull
  fi
  cd ../../
done
