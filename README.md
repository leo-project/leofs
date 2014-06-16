LeoFS - The Lion of Storage Systems
===================================

![LeoFS Logo](http://leo-project.net/leofs/assets/logos/leofs-logo-blk.svg "LeoFS")

LeoFS is a highly available, distributed, eventually consistent object/blob store. Organizations can use LeoFS to store lots of data efficiently, safely, and inexpensively.

We are supporting the following things:

* S3-API
  * LeoFS is an Amazon S3 compatible storage system.
  * Switch to LeoFS to decrease your cost from more expensive public-cloud solutions.
* Large Object
  * LeoFS can handle files with more than GB
* Multi Data Center Replication
  * LeoFS is a highly scalable, fault-tolerant distributed file system without SPOF.
  * LeoFS's cluster can be viewed as ONE-HUGE storage. It consists of a set of loosely connected nodes.
  * We can build a global scale storage system with easy operations

To access LeoFS server, we can use the following S3 clients.

* [DragonDisk](http://www.dragondisk.com/)
* [S3FS-C](http://www.leofs.org/docs/s3_client.html#getting-started-with-s3fs-c-ubuntu-12-04-lts)
* [s3cmd](http://www.leofs.org/docs/s3_client.html#connecting-to-leofs-using-s3cmd)

We can also access LeoFS using S3 libraries of each programming language.
We've prepared sample code for the following programming languages.

* [Java](http://www.leofs.org/docs/s3_client.html#getting-started-with-java-aws-sdk)
* [Ruby](http://www.leofs.org/docs/s3_client.html#getting-started-with-ruby-aws-sdk)
* [Python](http://www.leofs.org/docs/s3_client.html#getting-started-with-python-boto)
* [PHP](http://www.leofs.org/docs/s3_client.html#getting-started-with-php-aws-sdk)

GOALs
-------

* LeoFS aims to provide all of 3-HIGHs as follow:
  * HIGH Reliability
     * Nine nines - Operating ratios is 99.9999999%
  * High Scalability
     * Build huge-cluster at low cost
  * HIGH Cost Performance
     * Fast - Over 10Gbps
     * A lower cost than other storage
     * Provide easy management and easy operation

Further Reference
-------------------

* Please visit [our website](http://www.leofs.org/docs/).

Build LeoFS (For Developers)
----------------------------

You can install LeoFS from the [packages](http://www.leofs.org/#download_package).
Here, we explain how to build LeoFS from source.

First, you have to install the following packages to build Erlang and LeoFS.

```text
## [CentOS]
$ sudo yum install libuuid-devel cmake check check-devel
## [Ubuntu]
$ sudo apt-get install build-essential libtool libncurses5-dev libssl-dev cmake check
```

Then, install Erlang.

```text
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
## 2. Install Erlang (R16B03-1)
##
$ wget http://www.erlang.org/download/otp_src_R16B03-1.tar.gz
$ tar xzf otp_src_R16B03-1.tar.gz
$ cd otp_src_R16B03-1
$ ./configure --prefix=/usr/local/erlang/R16B03-1 \
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
    export ERL_HOME=/usr/local/erlang/R16B03-1
    export PATH=$PATH:$ERL_HOME/bin

$ source ~/.profile
```

Then, clone source of LeoFS and libraries from GitHub.

```text
$ git clone https://github.com/leo-project/leofs.git
$ cd leofs
$ git checkout -b develop remotes/origin/develop
$ ./rebar get-deps
$ ./git_checkout.sh develop
```

Then, build LeoFS with the following commands.

```text
$ make clean
$ make
$ make release
```

Now, you can find the LeoFS package as follow.

```text
$ ls package/
leo_gateway/  leo_manager_0/  leo_manager_1/  leo_storage/  README.md
```

Then, we can start and access LeoFS with the following commands.

```
$ package/leo_manager_0/bin/leo_manager start
$ package/leo_manager_1/bin/leo_manager start
$ package/leo_storage/bin/leo_storage start
$ package/leo_gateway/bin/leo_gateway start
$ telnet localhost 10010
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
status
[System config]
                System version : 1.0.0
                    Cluster Id : leofs_1
                         DC Id : dc_1
                Total replicas : 1
           # of successes of R : 1
           # of successes of W : 1
           # of successes of D : 1
 # of DC-awareness replicas    : 0
                     ring size : 2^128
             Current ring hash : 941f20fc
                Prev ring hash : 000000-1
[Multi DC replication settings]
         max # of joinable DCs : 2
            # of replicas a DC : 1

[Node(s) state]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | storage_0@127.0.0.1      | attached     |                |                | 2014-04-16 10:09:59 +0900
```

Build a LeoFS Cluster
---------------------

You can easily build a LeoFS cluster.

Please refer [here](http://www.leofs.org/docs/getting_started.html#quick-start-2-cluster).

Configure LeoFS
---------------

About the configuration of LeoFS, please refer [here](http://www.leofs.org/docs/configuration.html).

Benchmarking
------------

You can benchmark LeoFS with [Basho Bench](https://github.com/basho/basho_bench).

[Here](http://www.leofs.org/docs/benchmark.html) is a documentation to benchmark LeoFS.

Milestones
-----------

* *DONE* - 0.12 (Oct 2012 - Jan 2013)
    * Large Object Support (incl.Streaming/Multi-part/Range requests)
    * Web GUI-Console (LeoTamer - Optional)
        * Cluster manager/monitor
        * Log Analysis/Search
* *DONE* - 0.14 (Feb 2013 - Sep)
    * Multi-layer Cache (Using SSD)
    * Rack aware replica placement
    * Web GUI Console (Option)
       * Support whole LeoFS Manager's commands
* *DONE* - 0.16 (Oct 2013)
    * Increase compatibility S3-APIs#4
        * the bucket ACLs
    * Web GUI Console (Option)
       * Support whole LeoFS Manager's commands
* *On Going* - 1.0 (Nov 2013 - Apr 2014)
    * Multi Data Center Replication
    * Increase compatibility S3-APIs#5
        * Other bucket operations
    * QoS System Phase-1 (LeoInsight - Option)
       * Support *statistics/analyzer*
* 1.2 (May 2014 - Aug)
    * OpenStack Integration
        * Support for OpenStack Swift-API
    * Increase compatibility S3-APIs#6
        * Objects Expiration into the bucket
        * Versioning
    * Job Scheduler on the Manager
        * Support *auto-compaction*
    * QoS System Phase-2 (LeoInsight - Option)
       * Support *notifier*
    * Web GUI Console (Option)
        * LeoInsight(QoS) Integration
        * Support Log analysis/search
