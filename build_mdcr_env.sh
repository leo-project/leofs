#!/bin/sh
#======================================================================
#
# LeoFS
#
# Copyright (c) 2013-2014 Rakuten, Inc.
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
echo "*** LeoFS - Start building mdcr test-environment ***"

## Generate packages
rm -rf package/leo_*
rm -rf package/c1/leo_*
rm -rf package/c2/leo_*
mkdir package/c1
mkdir package/c2
make release

## Copy storage-files
cp -r package/leo_manager_* package/c1/
cp -r package/leo_storage package/c1/leo_storage_0
cp -r package/leo_storage package/c1/leo_storage_1
cp -r package/leo_storage package/c1/leo_storage_2
cp -r package/leo_gateway package/c1/leo_gateway_0

cp -r package/leo_manager_* package/c2/
cp -r package/leo_storage package/c2/leo_storage_0
cp -r package/leo_storage package/c2/leo_storage_1
cp -r package/leo_storage package/c2/leo_storage_2
cp -r package/leo_gateway package/c2/leo_gateway_0
rm -rf package/leo_manager_*
rm -rf package/leo_storage
rm -rf package/leo_gateway


cp priv/mdcr-test/c1/leo_manager.conf.0 package/c1/leo_manager_0/etc/leo_manager.conf
cp priv/mdcr-test/c1/leo_manager.conf.1 package/c1/leo_manager_1/etc/leo_manager.conf
cp priv/mdcr-test/c1/leo_storage_0.conf package/c1/leo_storage_0/etc/leo_storage.conf
cp priv/mdcr-test/c1/leo_storage_1.conf package/c1/leo_storage_1/etc/leo_storage.conf
cp priv/mdcr-test/c1/leo_storage_2.conf package/c1/leo_storage_2/etc/leo_storage.conf
cp priv/mdcr-test/c1/leo_gateway.conf   package/c1/leo_gateway_0/etc/leo_gateway.conf

cp priv/mdcr-test/c2/leo_manager.conf.0 package/c2/leo_manager_0/etc/leo_manager.conf
cp priv/mdcr-test/c2/leo_manager.conf.1 package/c2/leo_manager_1/etc/leo_manager.conf
cp priv/mdcr-test/c2/leo_storage_0.conf package/c2/leo_storage_0/etc/leo_storage.conf
cp priv/mdcr-test/c2/leo_storage_1.conf package/c2/leo_storage_1/etc/leo_storage.conf
cp priv/mdcr-test/c2/leo_storage_2.conf package/c2/leo_storage_2/etc/leo_storage.conf
cp priv/mdcr-test/c2/leo_gateway.conf   package/c2/leo_gateway_0/etc/leo_gateway.conf


## launch-C1
./package/c1/leo_manager_0/bin/leo_manager start
./package/c1/leo_manager_1/bin/leo_manager start
./package/c1/leo_storage_0/bin/leo_storage start
./package/c1/leo_storage_1/bin/leo_storage start
./package/c1/leo_storage_2/bin/leo_storage start
./package/c1/leo_gateway_0/bin/leo_gateway start

## launch-C2
./package/c2/leo_manager_0/bin/leo_manager start
./package/c2/leo_manager_1/bin/leo_manager start
./package/c2/leo_storage_0/bin/leo_storage start
./package/c2/leo_storage_1/bin/leo_storage start
./package/c2/leo_storage_2/bin/leo_storage start
./package/c2/leo_gateway_0/bin/leo_gateway start

echo "*** leofs - Finished :) ***"
