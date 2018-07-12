#!/bin/sh

CONTAINER_NAME=leo-nfs-tester
set -xe

docker pull leoproject/leofs-nfs-test:latest
docker run --rm --privileged=true --name $CONTAINER_NAME leoproject/leofs-nfs-test:latest /sbin/init &

# for safe
sleep 10

docker exec -it $CONTAINER_NAME ./run_nfs_test.sh
RET=$?
docker container stop $CONTAINER_NAME
return $RET
