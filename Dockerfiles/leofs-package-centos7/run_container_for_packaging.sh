#!/bin/sh

CONTAINER_NAME=leo-centos7-packager
set -xe

#docker pull leoproject/leofs-package-ubuntu16.04:latest
docker run --rm --privileged=true --name $CONTAINER_NAME leoproject/leofs-package-centos7:latest /sbin/init &

# for safe
sleep 10

docker exec -it $CONTAINER_NAME ./run_packaging.sh $@
RET=$?
docker container stop $CONTAINER_NAME
return $RET
