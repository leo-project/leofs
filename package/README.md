Welcome to LeoFS
=================

Overview
--------

LeoFS is the Web shaped object storage system and S3 compatible storage.

Getting Started
---------------

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

Quick Start
-------------

* The documentation is [here](http://www.leofs.org/docs/getting_started.html#quick-start)
* Support Clients
    * [Program Languages](http://www.leofs.org/docs/s3_client.html)
        * Java
        * Ruby
        * Python
        * PHP
    * [DragonDisk](http://www.dragondisk.com/)
    * [S3FS-C](http://www.leofs.org/docs/s3_client.html#getting-started-with-s3fs-c-ubuntu-12-04-lts)
    * [s3cmd](http://www.leofs.org/docs/s3_client.html#connecting-to-leofs-using-s3cmd)

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
* *DONE* 0.16 (Oct 2013)
    * Increase compatibility S3-APIs#4
        * the bucket ACLs
    * Web GUI Console (Option)
       * Support whole LeoFS Manager's commands
* 1.0 (Nov 2013 - Feb)
    * Multi Data Center Replication
    * Increase compatibility S3-APIs#5
        * Other bucket operations
    * QoS System Phase-1 (LeoInsight - Option)
       * Support *statistics/analyzer*
* 1.2 (Feb 2014 - May)
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
