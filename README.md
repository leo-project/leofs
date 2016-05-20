# LeoFS - The Lion of Storage Systems

[![Join the chat at https://gitter.im/leo-project/leofs](https://badges.gitter.im/leo-project/leofs.svg)](https://gitter.im/leo-project/leofs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

![LeoFS Logo](http://leo-project.net/leofs/docs/_static/leofs-logo-small.png)

## Overview

LeoFS is a highly available, distributed, eventually consistent object/blob store.
If you are searching a storage system that is able to store huge amount and various kind of files such as photo, movie, log data and so on, LeoFS is suitable for that.

LeoFS is supporting the following features:

* **Multi Protocol**
  * **S3-API Support**
      * LeoFS is an Amazon S3 compatible storage system.
      * Switch to LeoFS to decrease your cost from more expensive public-cloud solutions.
  * **NFS Support**
      * NFS support was provided from LeoFS v1.1, the current status of which is beta.
* **Large Object Support**
  * LeoFS can handle files with more than GB
* **Multi Data Center Replication**
  * LeoFS is a highly scalable, fault-tolerant distributed file system without SPOF.
  * LeoFS's cluster can be viewed as ONE-HUGE storage. It consists of a set of loosely connected nodes.
  * We can build a global scale storage system with easy operations

## Architecture

![leofs-architecture-1](http://leo-project.net/leofs/docs/_images/leofs-architecture.0012.jpg)

LeoFS consists of 3 applications - [LeoFS Storage](https://github.com/leo-project/leo_storage), [LeoFS Gateway](https://github.com/leo-project/leo_gateway) and [LeoFS Manager](https://github.com/leo-project/leo_manager) which depend on Erlang.

[LeoFS Gateway](https://github.com/leo-project/leo_gateway) handles http-request and http-response from any clients when using REST-API OR S3-API. Also, it is already built in the object-cache mechanism (memory and disk cache).

[LeoFS Storage](https://github.com/leo-project/leo_storage) handles GET, PUT and DELETE objects as well as metadata. Also, it has replicator, recoverer and queueing mechanism in order to keep running a storage node and realise eventual consistency.

[LeoFS Manager](https://github.com/leo-project/leo_manager) always monitors LeoFS Gateway and LeoFS Storage nodes. The main monitoring status are Node status and RING’s checksum in order to realise to keep high availability and keep data consistency.

We can access LeoFS server using <a target="_blank" href="http://www.leofs.org/docs/s3_client.html">S3 clients and S3 client libries of each programming language</a>.


## Slide

The presentation - <a href="https://www.slideshare.net/rakutentech/scaling-and-high-performance-storage-system-leofs" title="Scaling and High Performance Storage System: LeoFS" target="_blank">Scaling and High Performance Storage System: LeoFS</a>  was given at Erlang User Conference 2014 in Stockholm on June 2014

## GOALs

* LeoFS aims to provide all of 3-HIGHs as follow:
  * HIGH Reliability
     * Nine nines - Operating ratios is 99.9999999%
  * High Scalability
     * Build huge-cluster at low cost
  * HIGH Cost Performance
     * Fast - Over 10Gbps
     * A lower cost than other storage
     * Provide easy management and easy operation

## Further Reference

* <a target="_blank" href="http://leo-project.net/leofs/docs/">LeoFS Documentation</a>.


## Build LeoFS with LeoFS Packages

LeoFS packages have been already provided on the Web. You're able to easily install LeoFS on your environments.

* LeoProject
    * <a target="_blank" href="http://leo-project.net/leofs/download.html">CentOS-7.0</a>
    * <a target="_blank" href="http://leo-project.net/leofs/download.html">CentOS-6.x</a>
    * <a target="_blank" href="http://leo-project.net/leofs/download.html">14.04 and later</a>
* Community
    * <a target="_blank" href="http://www.freshports.org/databases/leofs">FreeBSD</a>

<a target="_blank" href="http://leo-project.net/leofs/docs/getting_started.html">Here</a> is the installation manual.


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
$ wget http://www.hpl.hp.com/research/linux/atomic_ops/download/libatomic_ops-7.2d.tar.gz
$ tar xzvf libatomic_ops-7.2d.tar.gz
$ cd libatomic_ops-7.2d
$ ./configure --prefix=/usr/local
$ make
$ sudo make install

##
## 2. Install Erlang (17.5)
##
$ wget http://www.erlang.org/download/otp_src_17.5.tar.gz
$ tar xzf otp_src_17.5.tar.gz
$ cd otp_src_17.5
$ ./configure --prefix=/usr/local/erlang/17.5 \
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
    export ERL_HOME=/usr/local/erlang/17.5
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
$ make clean
$ make
$ make release
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
                    system version | 1.2.16
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
  S    | storage_0@127.0.0.1      | running      | 3923d007       | 3923d007       | 2015-10-30 09:44:11 +0900
-------+--------------------------+--------------+----------------+----------------+----------------------------

$ ./leofs-adm start
OK

$ ./leofs-adm status
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.2.16
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
  S    | storage_0@127.0.0.1      | running      | 3923d007       | 3923d007       | 2015-10-30 09:44:11 +0900
  G    | gateway_0@127.0.0.1      | running      | 3923d007       | 3923d007       | 2015-10-30 09:45:27 +0900
-------+--------------------------+--------------+----------------+----------------+----------------------------


```

## Build a LeoFS Cluster

You can easily build a LeoFS cluster.

Please refer <a target="_blank" href="http://www.leofs.org/docs/getting_started.html#quick-start-2-cluster">here</a>.

## Configure LeoFS

About the configuration of LeoFS, please refer <a target="_blank" href="http://www.leofs.org/docs/configuration.html">here</a>.

## Benchmarking

You can benchmark LeoFS with <a target="_blank" href="https://github.com/basho/basho_bench">Basho Bench</a>.

<a target="_blank" href="http://www.leofs.org/docs/benchmark.html">Here</a> is a documentation to benchmark LeoFS.


## Integration Test

We're able to easily check LeoFS with <a target="_blank" href="https://github.com/leo-project/leofs_test2">leofs_test</a> whether LeoFS has issues or not before getting installed LeoFS in your dev/staging/production environment(s).


## Milestones

* *DONE* - v1.0 (Nov 2013 - May 2014)
    * Multi Data Center Replication
    * Increase compatibility S3-APIs#5
        * Other bucket operations
* *DONE* - v1.1
    * NFS v3 Support *(alpha)*
    * Improve Web GUI Console (Option)
* *DONE* - v1.2
    * NFS v3 Support *(beta)*
    * Watchdog
    * Auto-compaction
* *On Going* - v1.4
    * NFS v3 Support *(stable)*
        * Improve performance of the list objects
        * NFS lock-option support
    * Erasure Code
    * Improve Web GUI console *(option)*
    * Improve compatibility S3-APIs#6
        * AWS Signature v4 support
        * Objects Expiration into a bucket
        * Versioning
* v2.0
    * NFS v4 Support
    * Data Deduplication
    * QoS Support *(Option)*
    * Improve compatibility S3-APIs#7


## Sponsors

LeoProject/LeoFS is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and supported by [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).