#!/bin/sh

CONTAINER_NAME=leo-ubuntu16.04-packager
set -xe

docker pull leoproject/leofs-package-ubuntu16.04:latest
docker run --rm --privileged=true --name $CONTAINER_NAME leoproject/leofs-package-ubuntu16.04:latest /sbin/init &

# for safe
sleep 10

docker exec -it -e GITHUB_USER=$1 -e GITHUB_PASSWORD=$2 $CONTAINER_NAME ./run_packaging.sh $3
RET=$?
docker container stop $CONTAINER_NAME
return $RET
