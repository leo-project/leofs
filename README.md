Welcome to LeoFS
=================

Overview
--------

LeoFS is the Web shaped object storage system.

* Feature
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
$ make release
````

* Modify Configuration File: [detail](http://www.leofs.org/docs/install.html#set-up-leofs-s-system-configuration-only-leofs-manager)
* Open Manager Console: [detail](http://www.leofs.org/docs/admin_guide.html#system-operation)

```text
$ cd package/leofs
$ manager_0/bin/leo_manager start
$ manager_1/bin/leo_manager start
$ storage/bin/leo_storage start
.
.
.
$ gateway/bin/leo_gateway start
````

