# Building a LeoFS' cluster with Ansible
## Purpose

This tutorial teaches you how to easily build a LeoFS cluster. All steps will not be explained in detail, it is assumed you already know [how to setup a stand-alone LeoFS system](quick.md). This guide exists to help you get a cluster up and running quickly. We recommend that you read the LeoFS Configuration and Administration Guide to learn how to administer your LeoFS cluster. We hope that by reading this tutorial you will be able to get a cluster started as quickly as possible.


## Installs and Launches LeoFS with `leofs_ansible`

You can easily install LeoFS into your servers with using [leofs_ansible](https://github.com/leo-project/leofs_ansible/), perform following steps:

### Uses `leofs_ansible`

* Installs and sets up <a href="https://github.com/ansible/ansible" target="_blank">Ansible</a>
* <a href="https://github.com/leo-project/leofs_ansible/wiki/" target="_blank">leofs_ansible's documentation</a> is already published, you can follow it to install and launch a LeoFS storage system.

### An example of `hosts`
#### Manager

* A number of nodes: 2
* IP: 10.0.0.101, 10.0.0.102
* Name: M0@10.0.0.101, M1@10.0.0.102

#### Gateway

* A number of nodes: 1
* IP: 10.0.0.103
* Name: G0@10.0.0.103

#### Storage

* A number of nodes: 3
* IP: 10.0.0.104 .. 10.0.0.106
* Name: S0@10.0.0.104 .. S2@10.0.0.106

In this case, we configure basic properties and nodes of LeoManager, LeoStorage an LeoGateway. You need to configure those properties to suit your environment.

```text
##
## Please check roles/common/vars/leofs_releases for available versions
##
[all:vars]
leofs_version=1.3.2
build_temp_path="/tmp/leofs_builder"
build_install_path="/tmp/"
build_branch="master"
source="package"

[builder]
10.0.0.100

# nodename of leo_manager_0 and leo_manager_1 are set at group_vars/all
[leo_manager_0]
10.0.0.101

# nodename of leo_manager_0 and leo_manager_1 are set at group_vars/all
[leo_manager_1]
10.0.0.102

[leo_storage]
10.0.0.104 leofs_module_nodename=S0@10.0.0.104
10.0.0.105 leofs_module_nodename=S1@10.0.0.105
10.0.0.106 leofs_module_nodename=S2@10.0.0.106

[leo_gateway]
10.0.0.103 leofs_module_nodename=G0@10.0.0.103

[leofs_nodes:children]
leo_manager_0
leo_manager_1
leo_gateway
leo_storage
```

## Confirmation

Uses the `leofs-adm status` command to confirm current its status.

```
$ leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.3.2
                        cluster Id | leofs_1
                             DC Id | dc_1
                    Total replicas | 2
          number of successes of R | 1
          number of successes of W | 1
          number of successes of D | 1
 number of rack-awareness replicas | 0
                         ring size | 2^128
-----------------------------------+----------
 Multi DC replication settings
-----------------------------------+----------
        max number of joinable DCs | 2
           number of replicas a DC | 1
-----------------------------------+----------
 Manager RING hash
-----------------------------------+----------
                 current ring-hash | 3923d007
                previous ring-hash | 3923d007
-----------------------------------+----------

 [State of Node(s)]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | S0@10.0.0.104            | running      | 3923d007       | 3923d007       | 2017-01-30 12:32:48 +0900
  S    | S1@10.0.0.105            | running      | 3923d007       | 3923d007       | 2017-01-30 12:32:48 +0900
  S    | S2@10.0.0.106            | running      | 3923d007       | 3923d007       | 2017-01-30 12:32:48 +0900
  G    | G0@10.0.0.103            | running      | 3923d007       | 3923d007       | 2017-01-30 12:32:52 +0900
-------+--------------------------+--------------+----------------+----------------+----------------------------
```

## Retrieves both an access-key and a secret accesskey

To make a bucket for the test, you need to retrieve both an `access-key` and a `secret access-key` with the `leofs-adm create-user` command

```bash
$ leofs-adm create-user <YOUR-ID>
access-key-id: <ACCESS-KEY-ID>
secret-access-key: <SECRET-ACCESS-KEY-ID>
```

After that, you use S3-client(s) with those keys when you access the LeoFS storage system.

## Configure an endpoint

To make buckets and objects available from a host running S3-client(s), you need to add an endpoint also tweak settings for the name resolution.

### Assign a name to LeoGateway through DNS or /etc/hosts

In this example, we take the latter way.

```bash
$ sudo vi /etc/hosts
# Add the below line on every host you are supposed to run S3-client(s)
10.0.0.103 leo_gateway
```

### Add an endpoint through leofs-adm

```bash
$ leofs-adm add-endpoint leo_gateway
```

### Make a specific bucket and objects under the bucket available

You need to resolve the domain name `test.leo_gateway` into the IP address of LeoGateway if you are going to have a bucket named `test`.

```bash
$ sudo vi /etc/hosts
# Add the below line on every host you are supposed to run S3-client(s)
10.0.0.103 test.leo_gateway
```

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
## proxy_host = leo_gateway
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

You now have a working LeoFS cluster. Make sure to have a look at [Administrators / Settings]() to learn more about administration and settings of a LeoFS storage system.

## Related Links

- [Getting Started / Quick Installation and Setup](quick.md)
- [For Administrators / Setup / Planning for Production](/admin/setup/planning_for_production.md)
- [For Administrators / Settings / Environment Configuration](/admin/settings/environment_config.md)
- [For Administrators / Settings / Cluster Settings](/admin/settings/cluster.md)
- [For Administrators / Settings / LeoManager Settings](/admin/settings/leo_manager.md)
- [For Administrators / Settings / LeoStorage Settings](/admin/settings/leo_storage.md)
- [For Administrators / Settings / LeoGateway Settings](/admin/settings/leo_gateway.md)
- [For Administrators / System Operations / Cluster Operations](/admin/system_operations/cluster.md)
- [FAQ / LeoFS Clients](/faq/client.md)
