#!/bin/sh
#======================================================================
#
# LeoFS
#
# Copyright (c) 2012-2014 Rakuten, Inc.
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

if [ $# -ne 1 ]; then
    echo "No command to run specified!"
    echo "Usage: bootstrap build|start|stop"
    exit 1
fi

if [ $1 != "build" -a $1 != "start" -a $1 != "stop" ]; then
    echo "No command to run specified!"
    echo "Usage: bootstrap start|stop"
    exit 1
fi

## Stop the processes
package/leo_gateway_0/bin/leo_gateway stop
package/leo_storage_0/bin/leo_storage stop
package/leo_storage_1/bin/leo_storage stop
package/leo_storage_2/bin/leo_storage stop
package/leo_storage_3/bin/leo_storage stop
package/leo_manager_0/bin/leo_manager stop
package/leo_manager_1/bin/leo_manager stop
if [ $1 = "stop" ]; then
    exit 1
fi

## Generate the packages
rm -rf package/leo_*
./rebar compile
make release

## Copy the storage-files
cp -r package/leo_storage package/leo_storage_0
cp -r package/leo_storage package/leo_storage_1
cp -r package/leo_storage package/leo_storage_2
cp -r package/leo_storage package/leo_storage_3
cp -r package/leo_gateway package/leo_gateway_0
rm -rf package/leo_storage
rm -rf package/leo_gateway

cp priv/test/app-m0.conf package/leo_manager_0/etc/leo_manager.conf
cp priv/test/app-m1.conf package/leo_manager_1/etc/leo_manager.conf
cp priv/test/app-s0.conf package/leo_storage_0/etc/leo_storage.conf
cp priv/test/app-s1.conf package/leo_storage_1/etc/leo_storage.conf
cp priv/test/app-s2.conf package/leo_storage_2/etc/leo_storage.conf
cp priv/test/app-s3.conf package/leo_storage_3/etc/leo_storage.conf

## Launch the applications 
if [ $1 = "build" ]; then
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
