# <a href="http://leo-project.net/leofs/" target="_blank">LeoFS</a> - A Storage System for a <a href="https://en.wikipedia.org/wiki/Data_lake" target="_blank">Data Lake</a> and the Web

[![Join the chat at https://gitter.im/leo-project/leofs](https://badges.gitter.im/leo-project/leofs.svg)](https://gitter.im/leo-project/leofs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Release](https://img.shields.io/badge/release-v1.3.5-green.svg?style=flat)](https://github.com/leo-project/leofs/releases/tag/1.3.5) [![Build Status](https://travis-ci.org/leo-project/leofs.svg?branch=master)](http://travis-ci.org/leo-project/leofs)

![LeoFS Logo](https://leo-project.net/leofs/docs-old/_static/leofs-logo-small.png)

## Overview

LeoFS is an **Enterprise Open Source Storage**, and it is a highly available, distributed, eventually consistent object/blob store. If you are seeking a storage system that can store huge amount and various kind of files such as photo, movie, log data and so on, LeoFS is suitable for that.

LeoFS is supporting the following features:

* **Multi Protocol**
  * **S3-API Support**
      * LeoFS is an Amazon S3 compatible storage system.
      * Switch to LeoFS to decrease your cost from more expensive public-cloud solutions.
  * **REST-API Support**
      * To easily access LeoFS with [REST-API](https://leo-project.net/leofs/docs/admin/protocols/rest/)
  * **NFS Support**
      * NFS support was provided from LeoFS v1.1, the current status of which is beta.
* **Large Object Support**
  * LeoFS covers handling large size objects.
* **Multi Data Center Replication**
  * LeoFS is a highly scalable, fault-tolerant distributed file system without SPOF.
  * LeoFS's cluster can be viewed as a huge capacity storage. It consists of a set of loosely connected nodes.
  * We can build a global scale storage system with easy operations

## Architecture

![leofs-architecture-1](https://leo-project.net/leofs/docs/assets/leofs-architecture.0012.jpg)

LeoFS consists of three core components - [LeoStorage](https://leo-project.net/leofs/docs/architecture/leo_storage/), [LeoGateway](https://leo-project.net/leofs/docs/architecture/leo_gateway/) and [LeoManager](https://leo-project.net/leofs/docs/architecture/leo_manager/) which depend on Erlang.

[LeoGateway](https://leo-project.net/leofs/docs/architecture/leo_gateway/) handles http-request and http-response from any clients when using REST-API OR S3-API. Also, it is already built in the object-cache mechanism (memory and disk cache).

[LeoStorage](https://leo-project.net/leofs/docs/architecture/leo_storage/) handles GET, PUT and DELETE objects as well as metadata. Also, it has replicator, recoverer and queueing mechanism in order to keep running a storage node and realise eventual consistency.

[LeoManager](https://leo-project.net/leofs/docs/architecture/leo_manager/) always monitors LeoGateway and LeoStorage nodes. The main monitoring status are Node status and RING’s checksum in order to realise to keep high availability and keep data consistency.

You can access a LeoFS system using Amazon S3 clients and the SDK</a>.


## Slide

The presentation - <a href="https://www.slideshare.net/rakutentech/scaling-and-high-performance-storage-system-leofs" title="Scaling and High Performance Storage System: LeoFS" target="_blank">Scaling and High Performance Storage System: LeoFS</a>  was given at Erlang User Conference 2014 in Stockholm on June 2014

## GOALs

* LeoFS has been aiming to provide **high reliability**, **high scalability**, and **high cost performance ratio**:
  * HIGH Reliability
     * Nine nines - Operating ratios is 99.9999999%
  * High Scalability
     * Build huge-cluster at low cost
  * HIGH Cost Performance
     * Fast - Over 10Gbps
     * A lower cost than other storage
     * Provide easy management and easy operation

## Further Reference

* <a target="_blank" href="https://leo-project.net/leofs/docs/">LeoFS Documentation</a>.


## Build LeoFS with LeoFS Packages

LeoFS packages have been already provided on the Web. You're able to easily install LeoFS on your environments.

* LeoProject
    * <a target="_blank" href="https://leo-project.net/leofs/download.html">CentOS 6.x, 7.x</a>
    * <a target="_blank" href="https://leo-project.net/leofs/download.html">Ubuntu 14.04, 16.04</a>
* Community
    * <a target="_blank" href="http://www.freshports.org/databases/leofs">FreeBSD</a>

<a target="_blank" href="https://leo-project.net/leofs/docs/installation/quick/">Here</a> is the installation manual.


## Build LeoFS From Source (For Developers)

Here, we explain how to build LeoFS from source code.
First, you have to install the following packages to build Erlang and LeoFS.

```bash
## [CentOS]
$ sudo yum install libuuid-devel cmake check check-devel
## [Ubuntu]
$ sudo apt-get install build-essential libtool libncurses5-dev libssl-dev cmake check
```

Then, install Erlang.

```bash
##
## 1. Install libatomic
##
$ wget http://www.ivmaisoft.com/_bin/atomic_ops/libatomic_ops-7.4.4.tar.gz
$ tar xzvf libatomic_ops-7.4.4.tar.gz
$ cd libatomic_ops-7.4.4
$ ./configure --prefix=/usr/local
$ make
$ sudo make install

##
## 2. Install Erlang (18.3)
##
$ wget http://erlang.org/download/otp_src_18.3.tar.gz
$ tar xzf otp_src_18.3.tar.gz
$ cd otp_src_18.3
$ ./configure --prefix=/usr/local/erlang/18.3 \
              --enable-smp-support \
              --enable-m64-build \
              --enable-halfword-emulator \
              --enable-kernel-poll \
              --without-javac \
              --disable-native-libs \
              --disable-hipe \
              --disable-sctp \
              --enable-threads \
              --with-libatomic_ops=/usr/local
$ make
$ sudo make install

##
## 3. Set PATH
##
$ vi ~/.profile
    ## append the follows:
    export ERL_HOME=/usr/local/erlang/18.3
    export PATH=$PATH:$ERL_HOME/bin

$ source ~/.profile
```

Then, clone source of LeoFS and libraries from GitHub.

```bash
$ git clone https://github.com/leo-project/leofs.git
$ cd leofs
$ git checkout -b develop remotes/origin/develop
$ ./rebar get-deps
$ ./git_checkout.sh develop
```

Then, build LeoFS with the following commands.

```bash
$ make && make release_for_test
```

Now, you can find the LeoFS package as follow.

```bash
$ ls package/
leo_gateway/  leo_manager_0/  leo_manager_1/  leo_storage/  README.md
```

Then, we can start and access LeoFS with the following commands. Also, you're able to easily operate LeoFS with [leofs-adm](https://github.com/leo-project/leofs/blob/master/leofs-adm) script.

```bash
$ package/leo_manager_0/bin/leo_manager start
$ package/leo_manager_1/bin/leo_manager start
$ package/leo_storage/bin/leo_storage start
$ package/leo_gateway/bin/leo_gateway start
$ ./leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.3.4
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
                 current ring-hash |
                previous ring-hash |
-----------------------------------+----------

 [State of Node(s)]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | storage_0@127.0.0.1      | attached     |                |                | 2017-06-02 14:59:20 +0900
-------+--------------------------+--------------+----------------+----------------+----------------------------

$ ./leofs-adm start
OK

$ ./leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.3.4
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
  S    | storage_0@127.0.0.1      | running      | 433fe365       | 433fe365       | 2017-06-02 15:00:10 +0900
  G    | gateway_0@127.0.0.1      | running      | 433fe365       | 433fe365       | 2017-06-02 15:00:12 +0900
-------+--------------------------+--------------+----------------+----------------+----------------------------
```

## Build a LeoFS Cluster

You can easily build a LeoFS cluster. See <a target="_blank" href="https://leo-project.net/leofs/docs/installation/cluster/">here</a>.

## Configure LeoFS

About the configuration of LeoFS, See <a target="_blank" href="https://leo-project.net/leofs/docs/admin/settings/cluster/">here</a>.

## Benchmarking

You can benchmark LeoFS with <a target="_blank" href="https://github.com/basho/basho_bench">Basho Bench</a>, and <a target="_blank" href="https://leo-project.net/leofs/docs/benchmark/README/">here</a> is a documentation to benchmark LeoFS.


## Integration Test

You can test LeoFS with <a target="_blank" href="https://github.com/leo-project/leofs_test2">leofs_test</a> whether LeoFS has issues or not before getting installed LeoFS in your dev/staging/production environment(s).


## Milestones
### Version 1

* *DONE* - [v1.0](https://github.com/leo-project/leofs/releases/tag/1.0.2)
    * Multi Data Center Replication
    * Increase compatibility S3-APIs#5
        * Other bucket operations
* *DONE* - [v1.1](https://github.com/leo-project/leofs/releases/tag/1.1.5)
    * NFS v3 Support *(alpha)*
    * Improve Web GUI Console (Option)
* *DONE* - [v1.2](https://github.com/leo-project/leofs/releases/tag/1.2.22)
    * NFS v3 Support *(beta)*
    * Watchdog
    * Automated data-compaction
* *DONE* - v1.3
    * NFS v3 Support *(stable)*
    * Improve compatibility S3-APIs#6
        * [AWS Signature v4](http://docs.aws.amazon.com/general/latest/gr/sigv4_changes.html) support (v1.3.0)
        * Custom Metadata Support (v1.3.1)
* *WIP* - v1.4
    * Erasure Code
    * Improve compatibility S3-APIs#7
        * [Server Side Object Encryption](http://docs.aws.amazon.com/AmazonS3/latest/dev/serv-side-encryption.html)
    * Integration with distributed computing frameworks
        * Hadoop integration
        * Spark integration
* v1.5
    * Hinted Hand-off
    * LeoManager's cluster replaces from Erlang's mnesia dependensy to another implementation

### Version 2
* v2.0
    * NFS v3 Support *(stable)*
        * Improve performance of the list objects, `the ls command`
    * Improvement of the Multi Data Center Replication
    * Searching objects by a custom-metadata
    * Improve compatibility S3-APIs#8
        * Objects Expiration into a Bucket
        * Object Versioning
    * Improve Web GUI console, <a href="https://github.com/leo-project/leo_center" target="_blank">LeoFS Center</a> *(option)*
* v2.2
    * NFS v4 Support
    * Data Deduplication
    * Improve compatibility S3-APIs#8


## Versioning Policy

LeoFS adheres to [the versioning policy](http://semver.org/) from v1.3.3.


## Licensing

LeoFS is licensed under the Apache License, Version 2.0. See LICENSE for the full license text.


## Sponsors

LeoProject/LeoFS is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and supported by [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
