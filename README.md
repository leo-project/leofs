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
    * LeoFS's cluster consists of a set of loosely connected nodes. So, They can be viewed as "ONE-Huge Storage".
    * LeoFS is made by modular-system. So, It realize a lower cost of operations.
* S3 Compatible
    * LeoFS is Amazon S3 comapatible storage system.
    * If you think public-cloud's cost is HIGH, You can easily switch to LeoFS.
* 3-HIGHs
    * “LeoFS” is highly scalable, fault-tolerant "Distributed File System" for the Web.
    * Different than other DFS — “LeoFS” offers a number of unique benefits to users:
       * HIGH cost performance
       * HIGH Reliability
       * HIGH Scalability

Where to find more
-------------------

* Detail document is [here](http://www.leofs.org/docs/).

Quich Start
-------------

* Prepare
  * "leofs" uses the "rebar" build system. Makefile so that simply running "make" at the top level should work.
    * [rebar](https://github.com/basho/rebar)
  * "leofs" requires [Erlang R14B04](http://www.erlang.org/download_release/12).
* Build and Package

```text
$ git clone https://github.com/leo-project/leofs.git
$ cd leofs
$ make prepare
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
     * Fast - Over 200MB/sec into 10GE
     * A lower cost than other storage
     * Provide easy management and easy operation
  * HIGH Reliability
     * Nine nines - Operating ratios is 99.9999999%
  * High Scalability
     * Build Huge-Cluster at low cost

Milestones
-----------
* 0.9.1
  * Large Object Support (over 64MB)
  * Support [Cowboy](https://github.com/essen) on "[leo_gateway](https://github.com/leo-project/leo_gateway)"
  * Enhance S3-API (1)
     * Bucket-related
* 0.9.2
  * Enhance S3-API (2)
     * Authentication
  * Web-Console ([Leo Tamer](https://github.com/leo-project/leo_tamer))
     * Log Analysis/Search
     * File Manager
