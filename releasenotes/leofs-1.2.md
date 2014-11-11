1.2.1
=====

Features and Improvements for LeoFS
-----------------------------------

* Improvements
    * Supported [LeoCenter](https://github.com/leo-project/leo_center) for LeoFS v1.2

Bugs Fixed
-----------

* Fixed system migration issue from v1.x to v1.2.0

Used Libraries
---------------

* leo project
    * [leo_backend-db v1.1.3](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.6.0](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.1.0](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.11](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.1.3](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.4.1](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.2.0](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.1.8](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.9](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager 1.9.1](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.10](https://github.com/leo-project/leo_rpc.git)
    * [leo_pod v0.6.2](https://github.com/leo-project/leo_pod.git)
    * [leo_s3_libs v1.1.4](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.7](https://github.com/leo-project/leo_statistics.git)
    * [leo_watchdog v0.4.1](https://github.com/leo-project/leo_watchdog.git)
    * [savanna_agent v0.4.4](https://github.com/leo-project/savanna_agent.git)
    * [savanna_commons v0.8.6](https://github.com/leo-project/savanna_commons.git)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen.git)
    * [nfs_rpc_server v0.2.2](https://github.com/leo-project/nfs_rpc_server.git)
    * [leo_gateway v1.2.1](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.2.1](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.2.1](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.7.0](https://github.com/basho/bitcask.git)
    * [cowboy v1.0.0](https://github.com/extend/cowboy.git)
    * [cowlib v1.0.0](https://github.com/extend/cowboy.git)
    * [elarm v0.3.0](https://github.com/leo-project/elarm.git)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb.git)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


1.2.0
=====

Features and Improvements for LeoFS
-----------------------------------

* New Features
    * Watchdog mechnism has been implemented, which is called [leo_watchdog](https://github.com/leo-project/leo_watchdog)
        * Target resources: [cpu, io, disk]
        * Status: Beta
        * Requirements: CentOS-6.5 later and Ubuntu-13.10/14.04 later
        * Furure plan: Support FreeBSD and SmartOS with v1.2.1
* Improved
    * [#121](https://github.com/leo-project/leofs/issues/121) Tool to migrate metadata dbs from bitcask to leveldb
    * [#166](https://github.com/leo-project/leofs/issues/166) ``leo_rpc`` ``leo_storage`` Multi DC replication messages could lost
    * [#202](https://github.com/leo-project/leofs/issues/202) ``leo_gateway`` Made timeout configurable
    * [#239](https://github.com/leo-project/leofs/issues/239) ``leo_gateway`` Added custom HTTP header for CDN integration
    * [#255](https://github.com/leo-project/leofs/issues/255) ``leo_gateway`` Made a PUT request in parallel when handling a large object
    * [#256](https://github.com/leo-project/leofs/issues/256) ``leo_gateway`` Upgraded [Cowboy 1.0.0](https://github.com/ninenines/cowboy)
    * [#260](https://github.com/leo-project/leofs/issues/260) ``leo_gateway`` Made a MOVE request in parallel when handling a large object
    * ``leo_storage`` ``leo_mq`` Implemented to be able to operate a mq server of a storage internal
    * ``leo_storage`` Able to recursively remove objects of under directory with S3-Clients

Bugs Fixed
-----------

* [#262](https://github.com/leo-project/leofs/issues/262) ``leo_gateway`` ``leo_storage`` Improved RPC in order to be able to handle huge traffic with large size objects
* [#263](https://github.com/leo-project/leofs/issues/263) ``leo_gateway`` Wrong error code when creating a bucket that already exists
* [#265](https://github.com/leo-project/leofs/issues/265) ``leo_object_storage`` Fixed that some succeeded updates might be ignored silently while executing compaction
* [#268](https://github.com/leo-project/leofs/issues/265) ``leo_object_storage`` Fixed that wrong output format of data-diagnosis when including children of an large-object

Used Libraries
---------------

* leo project
    * [leo_backend-db v1.1.3](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.6.0](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.1.0](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.11](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.1.3](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.4.1](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.2.0](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.1.8](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.9](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager 1.9.1](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.10](https://github.com/leo-project/leo_rpc.git)
    * [leo_pod v0.6.2](https://github.com/leo-project/leo_pod.git)
    * [leo_s3_libs v1.1.4](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.7](https://github.com/leo-project/leo_statistics.git)
    * [leo_watchdog v0.4.1](https://github.com/leo-project/leo_watchdog.git)
    * [savanna_agent v0.4.4](https://github.com/leo-project/savanna_agent.git)
    * [savanna_commons v0.8.6](https://github.com/leo-project/savanna_commons.git)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen.git)
    * [nfs_rpc_server v0.2.2](https://github.com/leo-project/nfs_rpc_server.git)
    * [leo_gateway v1.2.0](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.2.0](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.2.0](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.7.0](https://github.com/basho/bitcask.git)
    * [cowboy v1.0.0](https://github.com/extend/cowboy.git)
    * [cowlib v1.0.0](https://github.com/extend/cowboy.git)
    * [elarm v0.3.0](https://github.com/leo-project/elarm.git)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb.git)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)
