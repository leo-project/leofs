#!/bin/sh

CONTAINER_NAME=leo-ubuntu18.04-packager
set -xe

#docker pull leoproject/leofs-package-ubuntu18.04:latest
docker run --rm --privileged=true --name $CONTAINER_NAME leoproject/leofs-package-ubuntu18.04:latest /sbin/init &

# for safe
sleep 10

docker exec -it $CONTAINER_NAME ./run_packaging.sh $@
RET=$?
docker container stop $CONTAINER_NAME
return $RET
