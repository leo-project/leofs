leofs-0.16.8
==============

Features and Improvements for LeoFS
-----------------------------------

* Improved
  * [#109](https://github.com/leo-project/leofs/issues/109) Moved to "ini-file" configuration, which no longer use Erlang-specific syntax. Using [cuttlefish](https://github.com/basho/cuttlefish) to realize this function.
  * [#110](https://github.com/leo-project/leofs/issues/110) Supported [erlcloud](https://github.com/gleber/erlcloud) for Erlang users
  * [Supported Mac OS X](https://github.com/leo-project/libcutil/pull/2) which was contributed from Jeff Li

Bugs Fixed
-----------

* [#102](https://github.com/leo-project/leofs/issues/102) Happened timeout when uploading a large object
* [#108](https://github.com/leo-project/leofs/issues/108) Happened not relocation of object(s) when removing a node and executing rebalance then add same node
* [#111](https://github.com/leo-project/leofs/issues/111) Could not upload large size file
* Fixed that not migrated "Bucket Table"
* Fixed that an exception of compaction happened rarely

Used Libraries
---------------

* leo project
    * [leo_backend-db v0.14.2](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.4.16](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v0.14.9](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.7](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v0.12.6](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.2.9](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v0.12.22](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v0.14.8](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.8.18](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager v1.2.5](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_s3_libs v0.12.24](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v0.14.7](https://github.com/leo-project/leo_statistics.git)
    * [leo_gateway v0.16.8](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v0.16.8](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v0.16.8](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.6.4](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [folsom](https://github.com/boundary/folsom.git)
    * [jiffy](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)

leofs-0.16.5
==============

Features and Improvements for LeoFS
-----------------------------------

* Improved
   * S3-API related
      * [#103] (https://github.com/leo-project/leofs/issues/103) Able to change password on Manager console
      * [#105] (https://github.com/leo-project/leofs/issues/105) Able to change an owner of a bucket
      * [#106] (https://github.com/leo-project/leofs/issues/106) Rename *set-endpoint* to *add-endpoint* in order to unify it and other commands
      * Multicast *update-acl* to Gateway's node(s)
      * Multicast *delete-bucket* to Gateway's node(s)
   * Other
      * [#98] (https://github.com/leo-project/leofs/issues/98) Made a bucket name compatible with the AWS-S3 naming rule of US-region
      * [#99] (https://github.com/leo-project/leofs/issues/99) Able to set log-directory of *leo-redundant-manager*
      * Revised that manage redundant nodes toward the multi data center replication
          * Supported that manage previous and current members in a cluster in order not to happen lost data during the period of relocation of data
          * Able to recover that Manager failed to send massage of relocation from Manager to Storage(s)
          * Reduced costs of relocation of data than previous version
      * Supported Ubuntu-13.10 (include Ubuntu Server 13.10)

Bugs Fixed
-----------

* [#94] (https://github.com/leo-project/leofs/issues/94) Happened an error when parsing XML document with S3's Java-Client
* [#95] (https://github.com/leo-project/leofs/issues/95) Could not resume storage node
* [#96] (https://github.com/leo-project/leofs/issues/96) Happened an error when Gateway is requested from *boto*
* [#97] (https://github.com/leo-project/leofs/issues/97) Could not upload an object when including a bucket name in a path
* [#100] (https://github.com/leo-project/leofs/issues/100) Broken records after update from 0.14.9 to 0.16.0

Used Libraries
---------------

* leo project
    * [leo_backend-db v0.14.1](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.4.14](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v0.14.8](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.5](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v0.12.5](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.2.7](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v0.12.21](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v0.14.7](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.8.17](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager v1.2.3](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_s3_libs v0.12.21](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v0.14.6](https://github.com/leo-project/leo_statistics.git)
    * [leo_gateway v0.16.5](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v0.16.5](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v0.16.5](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.6.2](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [folsom](https://github.com/boundary/folsom.git)
    * [jiffy](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


leofs-0.16.0
==============

Features and Improvements for LeoFS
-----------------------------------

* New Features
    * Gateway
        * Supported GET/PUT ACL a Bucket [S3-API]
        * (Beta) Able to output access-logs to Elasticsearch in order to realize to access-log analysis with Kibana
    * Other
        * Implemented SmartOS packager (Contributed by Heinz N. Gies - [Project FiFo](http://project-fifo.net/))
* Improved
    * Manager
        * Supported single 'Manager'
        * Supported to check # of detached node before executing detach-command

Used Libraries
---------------

* leo project
    * [leo_backend-db v0.14.1](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.4.13](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v0.14.8](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.4](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v0.12.5](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.2.6](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v0.12.20](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v0.14.7](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.8.17](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager v1.0.4](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_s3_libs v0.12.17](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v0.14.6](https://github.com/leo-project/leo_statistics.git)
    * [leo_gateway v0.16.0](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v0.16.0](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v0.16.0](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.6.2](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [folsom](https://github.com/boundary/folsom.git)
    * [jiffy](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)
