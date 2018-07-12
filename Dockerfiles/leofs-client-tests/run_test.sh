#!/bin/sh

ROOT_DIR=/root/leofs_client_tests

# Prerequsites
## 1. Run coupled with the service leofs through docker-compose
## 2. DNS names like testc.leofs should be resolved through aliases (alternative hostnames)
##    For more details, refer https://docs.docker.com/compose/compose-file/#aliases
##    OR links for docker-compose.yml version 1
##    For more details, refer https://docs.docker.com/compose/compose-file/compose-file-v1/#links

## Wait for the test LeoFS cluster up
until curl http://testc.s3.amazonaws.com:8080/path/to/file; [ $? -eq 0 ]; do
   sleep 5
done

## for safe
sleep 5

set -xe
(cd $ROOT_DIR/aws-sdk-cpp/build; ./LeoFSTest.cpp v4 s3.amazonaws.com 8080 testc)
(cd $ROOT_DIR/aws-sdk-go; go run ./LeoFSTest.go v4 s3.amazonaws.com 8080 testg)
(cd $ROOT_DIR/aws-sdk-java; ant -Dsignver=v4 -Dhost="s3.amazonaws.com" -Dport=8080 -Dbucket="testj")
(cd $ROOT_DIR/aws-sdk-php; php ./LeoFSTest.php v4 s3.amazonaws.com 8080 testp)
(cd $ROOT_DIR/aws-sdk-ruby; ruby ./LeoFSTest.rb v4 s3.amazonaws.com 8080 testr)
(cd $ROOT_DIR/boto; python ./LeoFSTest.py v4 s3.amazonaws.com 8080 testb)
(cd $ROOT_DIR/boto3; python ./LeoFSTest.py v4 s3.amazonaws.com 8080 testb3)
