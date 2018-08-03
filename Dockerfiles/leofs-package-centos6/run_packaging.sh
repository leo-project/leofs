#!/bin/sh

set -xe

# $1: The release tag will be set
# $2: The release-id will be set

. ~/erlang/19.3/activate

# 1. Do packaging
sh make_rpm.sh $1

# 2. Install the package
rpm -i ~/rpmbuild/RPMS/x86_64/leofs-${1}-1.x86_64.rpm
chsh -s /bin/bash leofs

# 3. start LeoFS
/usr/local/leofs/current/leo_manager_0/bin/leo_manager start
/usr/local/leofs/current/leo_manager_0/bin/leo_manager wait_ready

/usr/local/leofs/current/leo_manager_1/bin/leo_manager start
/usr/local/leofs/current/leo_manager_1/bin/leo_manager wait_ready

/usr/local/leofs/current/leo_storage/bin/leo_storage start
/usr/local/leofs/current/leo_gateway/bin/leo_gateway start
/usr/local/leofs/current/leo_storage/bin/leo_storage wait_ready
/usr/local/leofs/current/leo_gateway/bin/leo_gateway wait_ready

/usr/local/leofs/current/leofs-adm start
/usr/local/leofs/current/leofs-adm add-bucket test 05236

# 4. Upload the package as a release asset with release-id passed by a shell argument
# Refer to https://developer.github.com/v3/repos/releases/#upload-a-release-asset
# TODO
