#!/bin/sh
#======================================================================
#
# LeoFS
#
# Copyright (c) 2012-2013 Rakuten, Inc.
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

## Stop processes
package/leo_gateway_0/bin/leo_gateway stop
package/leo_storage_0/bin/leo_storage stop
package/leo_storage_1/bin/leo_storage stop
package/leo_storage_2/bin/leo_storage stop
package/leo_manager_0/bin/leo_manager stop
package/leo_manager_1/bin/leo_manager stop
if [ $1 = "stop" ]; then
    exit 1
fi

## Generate packages
rm -rf package/*
./rebar compile
make release

## Copy storage-files
cp -r package/leo_storage package/leo_storage_0
cp -r package/leo_storage package/leo_storage_1
cp -r package/leo_storage package/leo_storage_2
cp -r package/leo_gateway package/leo_gateway_0
rm -rf package/leo_storage
rm -rf package/leo_gateway

cp priv/test/app-m0.config package/leo_manager_0/etc/app.config
cp priv/test/app-s0.config package/leo_storage_0/etc/app.config
cp priv/test/app-s1.config package/leo_storage_1/etc/app.config
cp priv/test/app-s2.config package/leo_storage_2/etc/app.config

cp priv/test/vm-s0.config package/leo_storage_0/etc/vm.args
cp priv/test/vm-s1.config package/leo_storage_1/etc/vm.args
cp priv/test/vm-s2.config package/leo_storage_2/etc/vm.args

## launch
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

echo ":::"
echo "::: starting storages :::"
echo ":::"

sleep 30
./package/leo_gateway_0/bin/leo_gateway start

echo start | nc -C 127.0.0.1 10010
sleep 1
echo status | nc -C 127.0.0.1 10010

echo "*** leofs - Finished :) ***"
