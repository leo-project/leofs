#!/bin/sh

CONTAINER_NAME=leo-centos7-packager
set -xe

docker pull leoproject/leofs-package-centos7:latest
docker run --rm --privileged=true --name $CONTAINER_NAME leoproject/leofs-package-centos7:latest /sbin/init &

# for safe
sleep 10

docker exec -it -e GITHUB_USER=$1 -e GITHUB_PASSWORD=$2 $CONTAINER_NAME ./run_packaging.sh $3
RET=$?
docker container stop $CONTAINER_NAME
return $RET
