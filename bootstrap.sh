#!/bin/sh
#======================================================================
#
# LeoFS
#
# Copyright (c) 2012-2018 Rakuten, Inc.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
#======================================================================
TEST_INTEGRATION="integration-test"
TEST_WATCHDOG="watchdog-test"
TEST_CACHE="cache-test"
TEST_HTTP_CACHE="http-cache-test"
TEST_RACK_AWARENESS="rack-awareness-test"
USAGE="Usage: bootstrap build|start|stop $TEST_INTEGRATION|$TEST_WATCHDOG|$TEST_CACHE|$TEST_HTTP_CACHE|$TEST_RACK_AWARENESS"
ERROR_MSG="No command to run specified!"

if [ $# -ne 2 ]; then
    echo $ERROR_MSG
    echo $USAGE
    exit 1
fi

if [ $1 != "build" -a $1 != "start" -a $1 != "stop" ]; then
    echo $ERROR_MSG
    echo $USAGE
    exit 1
fi

if [ $2 != "$TEST_INTEGRATION" -a \
     $2 != "$TEST_WATCHDOG" -a \
     $2 != "$TEST_CACHE" -a \
     $2 != "$TEST_HTTP_CACHE" -a \
     $2 != "$TEST_RACK_AWARENESS" ]; then
    echo $ERROR_MSG
    echo $USAGE
    exit 1
fi

## Stop the processes
package/leo_gateway_0/bin/leo_gateway stop
package/leo_storage_0/bin/leo_storage stop
package/leo_storage_1/bin/leo_storage stop
package/leo_storage_2/bin/leo_storage stop
package/leo_storage_3/bin/leo_storage stop
package/leo_storage_4/bin/leo_storage stop
package/leo_manager_0/bin/leo_manager stop
package/leo_manager_1/bin/leo_manager stop
if [ $1 = "stop" ]; then
    exit 1
fi

## Generate the packages
make release_for_test

## Copy the storage-files
cp -r package/leo_storage package/leo_storage_0
cp -r package/leo_storage package/leo_storage_1
cp -r package/leo_storage package/leo_storage_2
cp -r package/leo_storage package/leo_storage_3
cp -r package/leo_storage package/leo_storage_4
cp -r package/leo_gateway package/leo_gateway_0
rm -rf package/leo_storage
rm -rf package/leo_gateway

if [ $2 = "$TEST_INTEGRATION" ]; then
    cp priv/test/integration-test/app-m0.conf package/leo_manager_0/etc/leo_manager.conf
    cp priv/test/integration-test/app-m1.conf package/leo_manager_1/etc/leo_manager.conf
    cp priv/test/integration-test/app-s0.conf package/leo_storage_0/etc/leo_storage.conf
    cp priv/test/integration-test/app-s1.conf package/leo_storage_1/etc/leo_storage.conf
    cp priv/test/integration-test/app-s2.conf package/leo_storage_2/etc/leo_storage.conf
    cp priv/test/integration-test/app-s3.conf package/leo_storage_3/etc/leo_storage.conf
    cp priv/test/integration-test/app-s4.conf package/leo_storage_4/etc/leo_storage.conf
    cp priv/test/integration-test/app-g0.conf package/leo_gateway_0/etc/leo_gateway.conf
elif [ $2 = "$TEST_WATCHDOG" ]; then
    cp priv/test/watchdog-test/app-m0.conf package/leo_manager_0/etc/leo_manager.conf
    cp priv/test/watchdog-test/app-m1.conf package/leo_manager_1/etc/leo_manager.conf
    cp priv/test/watchdog-test/app-s0.conf package/leo_storage_0/etc/leo_storage.conf
    cp priv/test/watchdog-test/app-s1.conf package/leo_storage_1/etc/leo_storage.conf
    cp priv/test/watchdog-test/app-s2.conf package/leo_storage_2/etc/leo_storage.conf
    cp priv/test/watchdog-test/app-s3.conf package/leo_storage_3/etc/leo_storage.conf
    cp priv/test/watchdog-test/app-s4.conf package/leo_storage_4/etc/leo_storage.conf
elif [ $2 = "$TEST_CACHE" ]; then
    cp priv/test/cache-test/app-m0.conf package/leo_manager_0/etc/leo_manager.conf
    cp priv/test/cache-test/app-m1.conf package/leo_manager_1/etc/leo_manager.conf
    cp priv/test/cache-test/app-s0.conf package/leo_storage_0/etc/leo_storage.conf
    cp priv/test/cache-test/app-s1.conf package/leo_storage_1/etc/leo_storage.conf
    cp priv/test/cache-test/app-s2.conf package/leo_storage_2/etc/leo_storage.conf
    cp priv/test/cache-test/app-s3.conf package/leo_storage_3/etc/leo_storage.conf
    cp priv/test/cache-test/app-s4.conf package/leo_storage_4/etc/leo_storage.conf
    cp priv/test/cache-test/app-g0.conf package/leo_gateway_0/etc/leo_gateway.conf
elif [ $2 = "$TEST_HTTP_CACHE" ]; then
    cp priv/test/http-cache-test/app-m0.conf package/leo_manager_0/etc/leo_manager.conf
    cp priv/test/http-cache-test/app-m1.conf package/leo_manager_1/etc/leo_manager.conf
    cp priv/test/http-cache-test/app-s0.conf package/leo_storage_0/etc/leo_storage.conf
    cp priv/test/http-cache-test/app-s1.conf package/leo_storage_1/etc/leo_storage.conf
    cp priv/test/http-cache-test/app-s2.conf package/leo_storage_2/etc/leo_storage.conf
    cp priv/test/http-cache-test/app-s3.conf package/leo_storage_3/etc/leo_storage.conf
    cp priv/test/http-cache-test/app-s4.conf package/leo_storage_4/etc/leo_storage.conf
    cp priv/test/http-cache-test/app-g0.conf package/leo_gateway_0/etc/leo_gateway.conf
elif [ $2 = "$TEST_RACK_AWARENESS" ]; then
    cp priv/test/rack-awareness-test/app-m0.conf package/leo_manager_0/etc/leo_manager.conf
    cp priv/test/rack-awareness-test/app-m1.conf package/leo_manager_1/etc/leo_manager.conf
    cp priv/test/rack-awareness-test/app-s0.conf package/leo_storage_0/etc/leo_storage.conf
    cp priv/test/rack-awareness-test/app-s1.conf package/leo_storage_1/etc/leo_storage.conf
    cp priv/test/rack-awareness-test/app-s2.conf package/leo_storage_2/etc/leo_storage.conf
    cp priv/test/rack-awareness-test/app-s3.conf package/leo_storage_3/etc/leo_storage.conf
    cp priv/test/rack-awareness-test/app-s3.conf package/leo_storage_3/etc/leo_storage.conf
    cp priv/test/rack-awareness-test/app-g0.conf package/leo_gateway_0/etc/leo_gateway.conf
else
    exit 1
fi

## Launch the applications
if [ $1 = "build" ]; then
    echo ""
    echo "*** leofs - Finished :) ***"
    echo ""
    exit 1
fi
./package/leo_manager_0/bin/leo_manager start
sleep 1
./package/leo_manager_0/bin/leo_manager wait_ready

./package/leo_manager_1/bin/leo_manager start
./package/leo_manager_1/bin/leo_manager wait_ready

./package/leo_storage_0/bin/leo_storage start
./package/leo_storage_1/bin/leo_storage start
./package/leo_storage_2/bin/leo_storage start
./package/leo_storage_3/bin/leo_storage start
./package/leo_storage_0/bin/leo_storage wait_ready
./package/leo_storage_1/bin/leo_storage wait_ready
./package/leo_storage_2/bin/leo_storage wait_ready
./package/leo_storage_3/bin/leo_storage wait_ready

echo ":::"
echo "::: Starting the storages :::"
echo ":::"
./package/leo_gateway_0/bin/leo_gateway start
./package/leo_gateway_0/bin/leo_gateway wait_ready

./leofs-adm start
sleep 1
./leofs-adm status
