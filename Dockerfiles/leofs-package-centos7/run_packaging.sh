#!/bin/sh

set -xe

# $1: The release tag will be set

. ~/erlang/19.3_systemd/activate

# 1. Do packaging
sh make_rpm.sh $1

# 2. Install the package
rpm -i ~/rpmbuild/RPMS/x86_64/leofs-${1}-1.x86_64.rpm
chsh -s /bin/bash leofs

# 3. start LeoFS through systemd
systemctl start leofs-manager-master
systemctl start leofs-manager-slave
systemctl start leofs-storage
systemctl start leofs-gateway

sleep 5

/usr/local/leofs/current/leofs-adm start
/usr/local/leofs/current/leofs-adm add-bucket test 05236

# 4. Upload the package as a release asset with release-id passed by a shell argument
# Refer to https://developer.github.com/v3/repos/releases/#upload-a-release-asset
git clone https://github.com/leo-project/leofs.git
cd leofs
mv ~/rpmbuild/RPMS/x86_64/leofs-${1}-1.x86_64.rpm leofs-${1}-1.el7.x86_64.rpm
hub release edit -m "" -a leofs-${1}-1.el7.x86_64.rpm $1
