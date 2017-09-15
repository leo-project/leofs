# Quick Installation and Setup

## Purpose

This section is a step by step guide to setting up LeoFS for the first time. By following this tutorial you're able to easily build a stand-alone LeoFS system.

## Note

In this section, you install LeoStorage, LeoGateway and LeoManager on a single system with no clustering to quickly understand LeoFS.

## Installation v1.3.3 or Later
### Ubuntu

For Ubuntu distributions, perform the following steps:

1. Downloads a LeoFS' package from [LeoFS' repository](https://github.com/leo-project/leofs/releases) on GitHub
2. Installs a LeoFS using `dpkg`

#### For Ubuntu 16.04
```text
$ wget https://github.com/leo-project/leofs/releases/download/{VERSION}/leofs_{VERSION}-1_erl-18.3_ubuntu-16.04_amd64.deb
$ sudo dpkg -i leofs_{VERSION}-1_erl-18.3_ubuntu-16.04_amd64.deb
$ ls -l /usr/local/leofs/
total 4
drwxr-xr-x 6 root root 4096 Sep  8 20:52 {VERSION}
lrwxrwxrwx 1 root root    5 Sep  8 20:33 current -> {VERSION}
```

#### For Ubuntu 14.04
```text
$ wget https://github.com/leo-project/leofs/releases/download/{VERSION}/leofs_{VERSION}-1_erl-18.3_ubuntu-14.04_amd64.deb
$ sudo dpkg -i leofs_{VERSION}-1_erl-18.3_ubuntu-14.04_amd64.deb
$ ls -l /usr/local/leofs/
total 4
drwxr-xr-x 6 root root 4096 Sep  8 20:52 {VERSION}
lrwxrwxrwx 1 root root    5 Sep  8 20:33 current -> {VERSION}
```

### CentOS

For CentOS distributions, perform the following steps:

1. Downloads a LeoFS' package from [LeoFS' repository](https://github.com/leo-project/leofs/releases) on GitHub
2. Installs a LeoFS using `rpm`

#### For CentOS 6.x

```text
$ wget https://github.com/leo-project/leofs/releases/download/{VERSION}/leofs-{VERSION}-1.erl-18.3.el6.x86_64.rpm
$ sudo rpm -ivh leofs-{VERSION}-1.erl-18.3.el6.x86_64.rpm
$ ls -l /usr/local/leofs/
total 4
drwxr-xr-x 6 root root 4096 Sep  8 20:52 {VERSION}
lrwxrwxrwx 1 root root    5 Sep  8 20:33 current -> {VERSION}
```

#### For CentOS 7.x

```text
$ wget https://github.com/leo-project/leofs/releases/download/{VERSION}/leofs-{VERSION}-1.erl-18.3.el7.x86_64.rpm
$ sudo rpm -ivh leofs-{VERSION}-1.erl-18.3.el7.x86_64.rpm
$ ls -l /usr/local/leofs/
total 4
drwxr-xr-x 6 root root 4096 Sep  8 20:52 {VERSION}
lrwxrwxrwx 1 root root    5 Sep  8 20:33 current -> {VERSION}
```

## Configuration

To be able to access the LeoFS storage system, you need to edit `/etc/hosts` which adheres to <a href="https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html#bucketnamingrules" target="_balnk">rules for bucket naming of S3-API</a>.

### Modifies “/etc/hosts”

* Adds a domain for the LeoFS bucket in /etc/hosts
* Bucket names must follow <a href="https://docs.aws.amazon.com/AmazonS3/latest/dev/BucketRestrictions.html#bucketnamingrules" target="_blank">these rules</a>

```text
## Replace {BUCKET_NAME} with the name of the bucket ##
$ sudo vi /etc/hosts
127.0.0.1 localhost {BUCKET_NAME}.localhost
```

## Launches LeoManager, LeoStorage, and LeoGateway node(s)

You launch the LeoFS storage system by the following steps:

1. There is only single replica by <a href="https://github.com/leo-project/leofs/blob/master/apps/leo_manager/priv/leo_manager_0.conf#L68" target="_blank">the default configuration of `consistency.num_of_replicas`</a>
2. Starts both `LeoManager master` and `LeoManager slave`
3. Starts a `LeoStorage`
4. Starts a `LeoGateway`

```text
$ cd /usr/local/leofs/{VERSION}
$ leo_manager_0/bin/leo_manager start
$ leo_manager_1/bin/leo_manager start
$ leo_storage/bin/leo_storage start
$ leo_gateway/bin/leo_gateway start
```

For systemd-based distributions running v1.4.0 or later, you can use systemd units to launch LeoFS instead:
```text
# systemctl start leofs-manager-master leofs-manager-slave leofs-gateway leofs-storage
```

!!! note "Note: Do not mix systemctl and launch scripts"
    Please make sure not to mix launching/stopping through systemctl and directly through launch scripts. More information is available at [For Administrators / System Operations / Systemd Services](/admin/system_operations/systemd.md#mixing-old-and-new-way-to-launch-nodes).


Then confirms whether the LeoManager nodes and the LeoStorage are running or not with the `leofs-adm status` command.

```text
$ leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.3.3
                        cluster Id | leofs_1
                             DC Id | dc_1
                    Total replicas | 1
          number of successes of R | 1
          number of successes of W | 1
          number of successes of D | 1
 number of rack-awareness replicas | 0
                         ring size | 2^128
-----------------------------------+----------
 Multi DC replication settings
-----------------------------------+----------
 [mdcr] max number of joinable DCs | 2
 [mdcr] total replicas per a DC    | 1
 [mdcr] number of successes of R   | 1
 [mdcr] number of successes of W   | 1
 [mdcr] number of successes of D   | 1
-----------------------------------+----------
 Manager RING hash
-----------------------------------+----------
                 current ring-hash | 433fe365
                previous ring-hash | 433fe365
-----------------------------------+----------

 [State of Node(s)]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | storage_0@127.0.0.1      | attached     |                |                | 2017-05-01 00:43:06 +0000
-------+--------------------------+--------------+----------------+----------------+----------------------------
```


### Starts the LeoFS storage system

If there is no issue, you're able to execute the `leofs-adm start` commmand to launch it, then confirm the current status with the `leof-adm status` command.

```text
$ ./leofs-adm start
Generating RING...
Generated RING
OK 100% - storage_0@127.0.0.1
OK

$ leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.3.3
                        cluster Id | leofs_1
                             DC Id | dc_1
                    Total replicas | 1
          number of successes of R | 1
          number of successes of W | 1
          number of successes of D | 1
 number of rack-awareness replicas | 0
                         ring size | 2^128
-----------------------------------+----------
 Multi DC replication settings
-----------------------------------+----------
 [mdcr] max number of joinable DCs | 2
 [mdcr] total replicas per a DC    | 1
 [mdcr] number of successes of R   | 1
 [mdcr] number of successes of W   | 1
 [mdcr] number of successes of D   | 1
-----------------------------------+----------
 Manager RING hash
-----------------------------------+----------
                 current ring-hash | 433fe365
                previous ring-hash | 433fe365
-----------------------------------+----------

 [State of Node(s)]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | storage_0@127.0.0.1      | running      | 433fe365       | 433fe365       | 2017-05-01 00:43:06 +0000
  G    | gateway_0@127.0.0.1      | running      | 433fe365       | 433fe365       | 2017-05-01 00:43:08 +0000
-------+--------------------------+--------------+----------------+----------------+----------------------------
```


## Retrieves both an access-key and a secret accesskey

To make a bucket for the test, you need to retrieve both an `access-key` and a `secret access-key` with the `leofs-adm create-user` command

```text
$ leofs-adm create-user <YOUR-ID>
access-key-id: <ACCESS-KEY-ID>
secret-access-key: <SECRET-ACCESS-KEY-ID>
```

After that, you use S3-client(s) with those keys when you access the LeoFS storage system.

### Uses`s3cmd` to access it

If you'd like to use <a href="http://s3tools.org/s3cmd" target="_blank">s3cmd</a> to access the LeoFS storage system, perform the following steps:

* Installs <a href="http://s3tools.org/s3cmd" target="_blank">s3cmd</a> on your machine
* Configures `s3cmd`

```bash
$ s3cmd --version
s3cmd version 1.6.1

$ s3cmd --configure
...
## access_key = <access-key-id>
## secret_key = <secret-access-key>
## proxy_host = localhost
## proxy_port = 8080
```

* Creates a bucket

```bash
$ s3cmd mb <BUCKET>
Bucket 's3://<BUCKET>/' created
```

* Puts an object into the bucket

```bash
$ s3cmd put /path/to/<OBJECT> s3://<BUCKET>/
upload: '<OBJET>' -> 's3://<BUCKET>/<OBJECT>'  [1 of 1]
 1096 of 1096   100% in    0s   170.92 kB/s  done
```

* Gets an object

```bash
$ s3cmd get s3://<BUCKET>/<OBJECT>
download: 's3://<BUCKET>/<OBJET>' -> './<OBJECT>'  [1 of 1]
 1096 of 1096   100% in    0s   307.38 kB/s  done
```

* Lists objects

```bash
$ s3cmd ls s3://<BUCKET>/
2017-01-30 02:24      1096   s3://<BUCKET>/<OBJECT>
```

* Removes an object

```bash
$ s3cmd del s3://<BUCKET>/<OBJECT>
delete: 's3://<BUCKET>/<OBJECT>'
```

## Wrap up

You now know how to setup a stand-alone LeoFS storage system, and make sure to have a look at [Building a LeoFS' cluster with Ansible](cluster.md) to learn how to setup a LeoFS cluster.


## Related Links

- [Getting Started / Building a LeoFS' cluster with Ansible](cluster.md)
- [For Administrators / Settings / Environment Configuration](/admin/settings/environment_config.md)
- [For Administrators / Settings / LeoManager Settings](/admin/settings/leo_manager.md)
- [For Administrators / Settings / LeoStorage Settings](/admin/settings/leo_storage.md)
- [For Administrators / Settings / LeoGateway Settings](/admin/settings/leo_gateway.md)
- [For Administrators / System Operations / Systemd Services](/admin/system_operations/systemd.md)
