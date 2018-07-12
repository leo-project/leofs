#!/bin/sh

set -xe

service rpcbind start

./package/leo_manager_0/bin/leo_manager start
sleep 1
./package/leo_manager_0/bin/leo_manager wait_ready

./package/leo_manager_1/bin/leo_manager start
./package/leo_manager_1/bin/leo_manager wait_ready

./package/leo_storage/bin/leo_storage start
./package/leo_gateway/bin/leo_gateway start
./package/leo_storage/bin/leo_storage wait_ready
./package/leo_gateway/bin/leo_gateway wait_ready

./leofs-adm start

sleep 3

./leofs-adm add-bucket test 05236
./leofs-adm gen-nfs-mnt-key test 05236 127.0.0.1

./apps/leo_gateway/test/leo_nfs_integration_tests.sh
