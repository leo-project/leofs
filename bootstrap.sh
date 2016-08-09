#!/bin/sh
#======================================================================
#
# LeoFS
#
# Copyright (c) 2012-2015 Rakuten, Inc.
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
echo "*** LeoFS - Start building test-environment ***"

if [ $# -ne 2 ]; then
    echo "No command to run specified!"
    echo "Usage: bootstrap build|start|stop integration-test|watchdog-test|cache-test|http-cache-test"
    exit 1
fi

if [ $1 != "build" -a $1 != "start" -a $1 != "stop" ]; then
    echo "No command to run specified!"
    echo "Usage: bootstrap build|start|stop integration-test|watchdog-test|cache-test|http-cache-test"
    exit 1
fi

if [ $2 != "integration-test" -a $2 != "watchdog-test" -a $2 != "cache-test" -a $2 != "http-cache-test" ]; then
    echo "No command to run specified!"
    echo "Usage: bootstrap build|start|stop integration-test|watchdog-test|cache-test|http-cache-test"
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
make release

## Copy the storage-files
cp -r package/leo_storage package/leo_storage_0
cp -r package/leo_storage package/leo_storage_1
cp -r package/leo_storage package/leo_storage_2
cp -r package/leo_storage package/leo_storage_3
cp -r package/leo_storage package/leo_storage_4
cp -r package/leo_gateway package/leo_gateway_0
rm -rf package/leo_storage
rm -rf package/leo_gateway

if [ $2 = "integration-test" ]; then
    cp priv/test/integration-test/app-m0.conf package/leo_manager_0/etc/leo_manager.conf
    cp priv/test/integration-test/app-m1.conf package/leo_manager_1/etc/leo_manager.conf
    cp priv/test/integration-test/app-s0.conf package/leo_storage_0/etc/leo_storage.conf
    cp priv/test/integration-test/app-s1.conf package/leo_storage_1/etc/leo_storage.conf
    cp priv/test/integration-test/app-s2.conf package/leo_storage_2/etc/leo_storage.conf
    cp priv/test/integration-test/app-s3.conf package/leo_storage_3/etc/leo_storage.conf
    cp priv/test/integration-test/app-s4.conf package/leo_storage_4/etc/leo_storage.conf
    cp priv/test/integration-test/app-g0.conf package/leo_gateway_0/etc/leo_gateway.conf
elif [ $2 = "watchdog-test" ]; then
    cp priv/test/watchdog-test/app-m0.conf package/leo_manager_0/etc/leo_manager.conf
    cp priv/test/watchdog-test/app-m1.conf package/leo_manager_1/etc/leo_manager.conf
    cp priv/test/watchdog-test/app-s0.conf package/leo_storage_0/etc/leo_storage.conf
    cp priv/test/watchdog-test/app-s1.conf package/leo_storage_1/etc/leo_storage.conf
    cp priv/test/watchdog-test/app-s2.conf package/leo_storage_2/etc/leo_storage.conf
    cp priv/test/watchdog-test/app-s3.conf package/leo_storage_3/etc/leo_storage.conf
    cp priv/test/watchdog-test/app-s4.conf package/leo_storage_4/etc/leo_storage.conf
elif [ $2 = "cache-test" ]; then
    cp priv/test/cache-test/app-m0.conf package/leo_manager_0/etc/leo_manager.conf
    cp priv/test/cache-test/app-m1.conf package/leo_manager_1/etc/leo_manager.conf
    cp priv/test/cache-test/app-s0.conf package/leo_storage_0/etc/leo_storage.conf
    cp priv/test/cache-test/app-s1.conf package/leo_storage_1/etc/leo_storage.conf
    cp priv/test/cache-test/app-s2.conf package/leo_storage_2/etc/leo_storage.conf
    cp priv/test/cache-test/app-s3.conf package/leo_storage_3/etc/leo_storage.conf
    cp priv/test/cache-test/app-s4.conf package/leo_storage_4/etc/leo_storage.conf
    cp priv/test/cache-test/app-g0.conf package/leo_gateway_0/etc/leo_gateway.conf
else
    cp priv/test/http-cache-test/app-m0.conf package/leo_manager_0/etc/leo_manager.conf
    cp priv/test/http-cache-test/app-m1.conf package/leo_manager_1/etc/leo_manager.conf
    cp priv/test/http-cache-test/app-s0.conf package/leo_storage_0/etc/leo_storage.conf
    cp priv/test/http-cache-test/app-s1.conf package/leo_storage_1/etc/leo_storage.conf
    cp priv/test/http-cache-test/app-s2.conf package/leo_storage_2/etc/leo_storage.conf
    cp priv/test/http-cache-test/app-s3.conf package/leo_storage_3/etc/leo_storage.conf
    cp priv/test/http-cache-test/app-s4.conf package/leo_storage_4/etc/leo_storage.conf
    cp priv/test/http-cache-test/app-g0.conf package/leo_gateway_0/etc/leo_gateway.conf
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
./package/leo_manager_1/bin/leo_manager start
sleep 1
./package/leo_storage_0/bin/leo_storage start
sleep 1
./package/leo_storage_1/bin/leo_storage start
sleep 1
./package/leo_storage_2/bin/leo_storage start
sleep 1
./package/leo_storage_3/bin/leo_storage start

echo ":::"
echo "::: Starting the storages :::"
echo ":::"
sleep 30
./package/leo_gateway_0/bin/leo_gateway start

./leofs-adm start
sleep 1
./leofs-adm status

echo "*** leofs - Finished :) ***"
