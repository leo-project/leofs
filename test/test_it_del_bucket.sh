#!/bin/sh
# -*- tab-width:4;indent-tabs-mode:nil -*-
# ex: ts=4 sw=4 et
#======================================================================
#
# LeoFS / A Test Tool of Deletion Bucket for #725, #150
#
# Copyright (c) 2017 Rakuten, Inc.
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

# Prepare this test
./bootstrap.sh start integration-test
sleep 5

# Put objects under 'test'
s3cmd mb s3://test_1
s3cmd mb s3://test_2
s3cmd mb s3://test_3
s3cmd mb s3://test_3
s3cmd mb s3://test_4
s3cmd mb s3://test_5
s3cmd mb s3://test_6
s3cmd mb s3://test_7
s3cmd mb s3://test_8
s3cmd mb s3://test_9
s3cmd mb s3://test_10
s3cmd mb s3://test_11
s3cmd mb s3://test_12
s3cmd sync deps/leo_commons s3://test_1/
s3cmd sync deps/leo_redundant_manager s3://test_2/
s3cmd sync deps/leo_backend_db s3://test_3/
s3cmd sync deps/leo_logger s3://test_4/
s3cmd sync deps/leo_object_storage s3://test_5/
s3cmd sync deps/leo_ordning_reda s3://test_6/
s3cmd sync deps/leo_redundant_manager s3://test_7/
s3cmd sync deps/leo_s3_libs s3://test_8/
s3cmd sync deps/leo_watchdog s3://test_9/
s3cmd sync deps/leo_commons s3://test_10/
s3cmd sync deps/leo_redundant_manager s3://test_11/
s3cmd sync deps/leo_backend_db s3://test_12/

# Remove one bucket
s3cmd rb s3://test_1
sleep 1
s3cmd rb s3://test_2
sleep 1
s3cmd rb s3://test_3
sleep 1
s3cmd rb s3://test_4
sleep 1
s3cmd rb s3://test_5
sleep 1
s3cmd rb s3://test_6
sleep 1
s3cmd rb s3://test_7
sleep 1
s3cmd rb s3://test_8
sleep 1
s3cmd rb s3://test_9
s3cmd rb s3://test_10
s3cmd rb s3://test_11
s3cmd rb s3://test_12

# Monitor the state of the del-bucket
watch ./leofs-adm delete-bucket-stats
