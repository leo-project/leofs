Welcome to LeoFS
=================

Overview
--------

LeoFS is the Web shaped object storage system and S3 compatible storage.

Feature
--------

* One-Huge Storage
    * LeoFS is the Web shaped object storage system.
    * LeoFS is built to operate in highly distributed environments, such as the private cloud.
    * LeoFS has NO SPOF.
    * LeoFS's cluster consists of a set of loosely connected nodes. So, They can be viewed as ONE-Huge Storage.
    * LeoFS is made by modular-system. So, It realizes a lower cost of operations.
* 3-HIGHs
    * LeoFS is highly scalable, fault-tolerant Distributed File System for the Web.
    * Different than other DFS — LeoFS offers a number of unique benefits to users:
       * HIGH cost performance
       * HIGH Reliability
       * HIGH Scalability
* S3 Compatible
    * LeoFS is Amazon S3 compatible storage system.
    * Switch to LeoFS to decrease your cost from more expensive public-cloud solution.


Where to find more
-------------------

* Detail document is [here](http://www.leofs.org/docs/).

Quick Start
-------------

* Prepare
  * "leofs" uses the "rebar" build system. Makefile so that simply running "make" at the top level should work.
    * [rebar](https://github.com/basho/rebar)
  * "leofs" requires [Erlang R14B04](http://www.erlang.org/download_release/12).
* Build and Package

```text
$ git clone https://github.com/leo-project/leofs.git
$ cd leofs
$ make
$ make release
````

* Modify Configuration File: [detail](http://www.leofs.org/docs/install.html#set-up-leofs-s-system-configuration-only-leofs-manager)
* Operate on "Manager Console": [detail](http://www.leofs.org/docs/admin_guide.html#system-operation)

```text
## Need to modify configuration files - 
##     manager_master: leofs/package/leofs/manager_0/etc/app.config
##      manager_slave: leofs/package/leofs/manager_1/etc/app.config
##            storage: leofs/package/leofs/storage/etc/app.config
##            gateway: leofs/package/leofs/storage/etc/app.config

$ cd package/leofs
$ manager_0/bin/leo_manager start
$ manager_1/bin/leo_manager start
$ storage/bin/leo_storage start

## Need to operate on "LeoFS-Manager's Console" - 
##     - Command: [ATTACH, START, STATUS]

$ gateway/bin/leo_gateway start

## Confirm LeoFS's Status on "LeoFS-Manager's Console" - 
##     - Command: [STATUS]
````

* Example - PUT an Object into LeoFS

```text
curl -v -X PUT -H "Content-Type: image/jpeg" --data-binary @stockholm-0.jpg http://localhost:8080/swe/stockholm-0.jpg
```

* Example - GET an Object from LeoFS

```text
curl -v http://localhost:8080/swe/stockholm-0.jpg > stockholm-0-1.jpg
```

GOALs
-------
* LeoFS aims to provide the following advantages:
  * HIGH Cost Performance
     * Fast - Over 200MB/sec into 10GE (READ)
     * A lower cost than other storage
     * Provide easy management and easy operation
  * HIGH Reliability
     * Nine nines - Operating ratios is 99.9999999%
  * High Scalability
     * Build Huge-Cluster at low cost

Milestones
-----------

* 0.10 (Aug 2012)
    * Enhance S3-API
        * Authentication
        * Bucket-related
* 0.12 (Oct 2012)
    * Large Object Support
    * Support Cowboy on "leo_gateway"
    * Web-Console (Leo Tamer)
        * Log Analysis/Search
* 0.14 (Dec 2012)
    * Multi-layer Cache (Using SSD)
    * Streaming
    * Web-Console (Leo Tamer)
        * Cluster manager/monitor
* 0.16 (2013)
    * HBase integration
        * Distributed Lock Mechanism

