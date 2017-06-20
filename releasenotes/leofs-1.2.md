# 1.2.22
## Improvements

* [#472](https://github.com/leo-project/leofs/issues/472) ``leo_storage`` Possibility to become imbalance of a total mq's msgs during a rebalance (detach-node)
* [#473](https://github.com/leo-project/leofs/issues/473) ``leo_manager`` Enforced **"-smp enabled"** by default
* [#474](https://github.com/leo-project/leofs/issues/474) ``leo_storage`` Improved the performance of the recover-node

## Used Libraries

* leo project
    * [leo_backend-db v1.2.2](https://github.com/leo-project/leo_backend_db/releases/tag/1.2.2)
    * [leo_cache v0.6.14](https://github.com/leo-project/leo_cache/releases/tag/0.6.14)
    * [leo_commons v1.1.6](https://github.com/leo-project/leo_commons/releases/tag/1.1.6)
    * [leo_dcerl v0.4.4](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.4)
    * [leo_logger v1.2.2](https://github.com/leo-project/leo_logger/releases/tag/1.2.2)
    * [leo_mcerl v0.6.2](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.2)
    * [leo_mq v1.4.2](https://github.com/leo-project/leo_mq/releases/tag/1.4.5)
    * [leo_object_storage v1.2.22](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.22)
    * [leo_ordning_reda v1.2.0](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.2.0)
    * [leo_redundant_manager 1.9.26](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.26)
    * [leo_rpc v0.10.8](https://github.com/leo-project/leo_rpc/releases/tag/0.10.8)
    * [leo_pod v0.6.6](https://github.com/leo-project/leo_pod/releases/tag/0.6.6)
    * [leo_s3_libs v1.1.13](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.13)
    * [leo_statistics v1.1.12](https://github.com/leo-project/leo_statistics/releases/tag/1.1.12)
    * [leo_watchdog v0.12.2](https://github.com/leo-project/leo_watchdog/releases/tag/0.12.2)
    * [savanna_agent v0.4.15](https://github.com/leo-project/savanna_agent/releases/tag/0.4.15)
    * [savanna_commons v0.10.1](https://github.com/leo-project/savanna_commons/releases/tag/0.10.1)
    * [erpcgen v0.2.4](https://github.com/leo-project/erpcgen/releases/tag/0.2.4)
    * [nfs_rpc_server v0.2.4](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.4)
    * [leo_gateway v1.2.22](https://github.com/leo-project/leo_gateway/releases/tag/1.2.22)
    * [leo_manager v1.2.22](https://github.com/leo-project/leo_manager/releases/tag/1.2.22)
    * [leo_storage v1.2.22](https://github.com/leo-project/leo_storage/releases/tag/1.2.22)
* others
    * [bitcask v2.0.1](https://github.com/basho/bitcask/releases/tag/2.0.1)
    * [cowboy v1.0.0](https://github.com/leo-project/cowboy/releases/tag/1.0.0-p1)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v2.1.10](https://github.com/basho/eleveldb/releases/tag/2.1.10)
    * [folsom v0.8.2-for-leofs](https://github.com/leo-project/folsom/releases/tag/0.8.2-for-leofs)
    * [jiffy v0.14.7](https://github.com/davisp/jiffy/releases/tag/0.14.7)
    * [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.2.21
## Bugs Fixed

* [#469](https://github.com/leo-project/leofs/issues/469) ``leo_gateway`` ``leo_cache`` Weird problem when getting files ~25MB
    * Could not handle a cached object correctly
    * Reduced amount of error logs which is related to the cache feature

## Used Libraries

* leo project
    * [leo_backend-db v1.2.1](https://github.com/leo-project/leo_backend_db/releases/tag/1.2.1)
    * [leo_cache v0.6.14](https://github.com/leo-project/leo_cache/releases/tag/0.6.14)
    * [leo_commons v1.1.6](https://github.com/leo-project/leo_commons/releases/tag/1.1.6)
    * [leo_dcerl v0.4.4](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.4)
    * [leo_logger v1.2.2](https://github.com/leo-project/leo_logger/releases/tag/1.2.2)
    * [leo_mcerl v0.6.2](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.2)
    * [leo_mq v1.4.2](https://github.com/leo-project/leo_mq/releases/tag/1.4.2)
    * [leo_object_storage v1.2.21](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.21)
    * [leo_ordning_reda v1.1.5](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.1.5)
    * [leo_redundant_manager 1.9.25](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.25)
    * [leo_rpc v0.10.7](https://github.com/leo-project/leo_rpc/releases/tag/0.10.7)
    * [leo_pod v0.6.4](https://github.com/leo-project/leo_pod/releases/tag/0.6.4)
    * [leo_s3_libs v1.1.12](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.12)
    * [leo_statistics v1.1.11](https://github.com/leo-project/leo_statistics/releases/tag/1.1.11)
    * [leo_watchdog v0.12.2](https://github.com/leo-project/leo_watchdog/releases/tag/0.12.2)
    * [savanna_agent v0.4.13](https://github.com/leo-project/savanna_agent/releases/tag/0.4.13)
    * [savanna_commons v0.10.0](https://github.com/leo-project/savanna_commons/releases/tag/0.10.0)
    * [erpcgen v0.2.4](https://github.com/leo-project/erpcgen/releases/tag/0.2.4)
    * [nfs_rpc_server v0.2.4](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.4)
    * [leo_gateway v1.2.21](https://github.com/leo-project/leo_gateway/releases/tag/1.2.21)
    * [leo_manager v1.2.21](https://github.com/leo-project/leo_manager/releases/tag/1.2.21)
    * [leo_storage v1.2.21](https://github.com/leo-project/leo_storage/releases/tag/1.2.21)
* others
    * [bitcask v2.0.1](https://github.com/basho/bitcask/releases/tag/2.0.1)
    * [cowboy v1.0.0](https://github.com/leo-project/cowboy/releases/tag/1.0.0-p1)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v2.1.10](https://github.com/basho/eleveldb/releases/tag/2.1.10)
    * [folsom v0.8.2-for-leofs](https://github.com/leo-project/folsom/releases/tag/0.8.2-for-leofs)
    * [jiffy v0.14.7](https://github.com/davisp/jiffy/releases/tag/0.14.7)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2)
    * [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.2.20
## Improvements

* [#389](https://github.com/leo-project/leofs/issues/389) ``erlang`` To be able to build LeoFS w/Erlang 18
* [#442](https://github.com/leo-project/leofs/issues/442) ``leo_storage`` When removing a large size object, a high-latency issue can be happened
* [#445](https://github.com/leo-project/leofs/issues/445) ``leo_backend_db`` May take too much time when restarting leo_storage
* [#446](https://github.com/leo-project/leofs/issues/446) ``leo_object_storage`` Unnecessary a file:position call when opening a AVS file
* [#447](https://github.com/leo-project/leofs/issues/447) There are some possibilities that some supervisours don't restart their child as we expect
* [#448](https://github.com/leo-project/leofs/issues/448) ``leo_storage``,``leo_mq`` Reduce queue messages
* Improve the LeoStorage's read performance
    * [#450](https://github.com/leo-project/leofs/issues/450) ``leo_object_storage`` Improve concurrency
    * [#467](https://github.com/leo-project/leofs/issues/467) ``leo_object_storage`` Improve concurrency of read operations

```bash
## command history file:
$ tree -vhD /path/to/leofs/leofs-1.2.20/leo_manager_0/log/app/
./package/leo_manager_0/log/app/
├── [ 103 Mar 17 16:40]  cmd_history -> /path/to/leofs/leofs-1.2.20/leo_manager_0/log/app/cmd_history.20160317.16.1
├── [ 230 Mar 17 16:41]  cmd_history.20160317.16.1
├── [  97 Mar 17 16:40]  error -> /path/to/leofs/leofs-1.2.20/leo_manager_0/log/app/error.20160317.16.1
├── [ 256 Mar 17 16:41]  error.20160317.16.1
├── [  96 Mar 17 16:40]  info -> /path/to/leofs/leofs-1.2.20/leo_manager_0/log/app/info.20160317.16.1
└── [1.6K Mar 17 16:40]  info.20160317.16.1

## command history format:
## | <datetime> | <unixtime> | <command> |
$ tail -f /path/to/leofs/leofs-1.2.20/leo_manager_0/log/app/cmd_history
2016-03-17 16:40:42.685451 +0900    1452670842685495    start
2016-03-17 16:41:22.732225 +0900    1452670882732267    update-log-level storage_0@127.0.0.1 debug
2016-03-17 16:41:36.745973 +0900    1452670896746008    update-consistency-level 1 1 1
```

## Bugs Fixed

* [#443](https://github.com/leo-project/leofs/issues/443) ``leo_storage`` Node stopped with many small write requests
* [#452](https://github.com/leo-project/leofs/issues/452) ``leo_gateway`` Cannot handle http requests when ``cache.http_cache`` is set to true also ``protocol=rest``
* [#453](https://github.com/leo-project/leofs/issues/453) ``leo_manager`` LeoManager may crash when # of histories is pretty large
* [#455](https://github.com/leo-project/leofs/issues/455) ``leo_storage`` Compaction is not executed when some nodes are empty
* [#458](https://github.com/leo-project/leofs/issues/458) ``leo_statistics``,``snmpa`` Value of a number of requests cannot clear
* [#463](https://github.com/leo-project/leofs/issues/463) ``leo_gateway``,``snmpa`` Can not retrieve the object-cache statistics
* [$468](https://github.com/leo-project/leofs/issues/468) ``leo_storage``,``leo_redundant_manager`` Incorrect ring after executing the takeover

## Used Libraries

* leo project
    * [leo_backend-db v1.2.1](https://github.com/leo-project/leo_backend_db/releases/tag/1.2.1)
    * [leo_cache v0.6.12](https://github.com/leo-project/leo_cache/releases/tag/0.6.12)
    * [leo_commons v1.1.6](https://github.com/leo-project/leo_commons/releases/tag/1.1.6)
    * [leo_dcerl v0.4.2](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.2)
    * [leo_logger v1.2.2](https://github.com/leo-project/leo_logger/releases/tag/1.2.2)
    * [leo_mcerl v0.6.2](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.2)
    * [leo_mq v1.4.2](https://github.com/leo-project/leo_mq/releases/tag/1.4.2)
    * [leo_object_storage v1.2.21](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.21)
    * [leo_ordning_reda v1.1.5](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.1.5)
    * [leo_redundant_manager 1.9.25](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.25)
    * [leo_rpc v0.10.7](https://github.com/leo-project/leo_rpc/releases/tag/0.10.7)
    * [leo_pod v0.6.4](https://github.com/leo-project/leo_pod/releases/tag/0.6.4)
    * [leo_s3_libs v1.1.12](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.12)
    * [leo_statistics v1.1.11](https://github.com/leo-project/leo_statistics/releases/tag/1.1.11)
    * [leo_watchdog v0.12.2](https://github.com/leo-project/leo_watchdog/releases/tag/0.12.2)
    * [savanna_agent v0.4.13](https://github.com/leo-project/savanna_agent/releases/tag/0.4.13)
    * [savanna_commons v0.10.0](https://github.com/leo-project/savanna_commons/releases/tag/0.10.0)
    * [erpcgen v0.2.4](https://github.com/leo-project/erpcgen/releases/tag/0.2.4)
    * [nfs_rpc_server v0.2.4](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.4)
    * [leo_gateway v1.2.20](https://github.com/leo-project/leo_gateway/releases/tag/1.2.20)
    * [leo_manager v1.2.20](https://github.com/leo-project/leo_manager/releases/tag/1.2.20)
    * [leo_storage v1.2.20](https://github.com/leo-project/leo_storage/releases/tag/1.2.20)
* others
    * [bitcask v2.0.1](https://github.com/basho/bitcask/releases/tag/2.0.1)
    * [cowboy v1.0.0](https://github.com/leo-project/cowboy/releases/tag/1.0.0-p1)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v2.1.10](https://github.com/basho/eleveldb/releases/tag/2.1.10)
    * [folsom v0.8.2-for-leofs](https://github.com/leo-project/folsom/releases/tag/0.8.2-for-leofs)
    * [jiffy v0.14.7](https://github.com/davisp/jiffy/releases/tag/0.14.7)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2)
    * [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.2.18
## Features and Improvements

* [#426](https://github.com/leo-project/leofs/issues/426) ``leo_storage`` Should be logged with warning or error level in some cases
* [#434](https://github.com/leo-project/leofs/issues/434) ``leo_manager`` Enable to reconfigure a consistency level
* [#435](https://github.com/leo-project/leofs/issues/435) ``leo_manager`` Enable to reconfigure a log level of storage and gateway node(s)
* [#437](https://github.com/leo-project/leofs/issues/437) ``leo_mq`` Replace bitcask used by leo_mq behind the scene with an appropriate one

## Bugs Fixed

* [#190](https://github.com/leo-project/leofs/issues/190) ``leo_gateway`` ``leo_storage`` Multipart uploads of large files produces partially corrupted data when upload chunk size
* [#427](https://github.com/leo-project/leofs/issues/427) ``leo_storage`` Need to fix the possibility of infinite loop w/the recover-node or the rebalance feature
* [#438](https://github.com/leo-project/leofs/issues/438) ``leo_s3_libs`` Requests to leo_gateway take too much time when leo_manager is down
* [#439](https://github.com/leo-project/leofs/issues/439) ``mdc-replication`` ``leo_redundant_manager`` Failed upgrading leo_cluster_info table from 1.2.x to 1.2.x

## Used Libraries

* leo project
    * [leo_backend-db v1.1.14](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.15)
    * [leo_cache v0.6.10](https://github.com/leo-project/leo_cache/tag/0.6.10)
    * [leo_commons v1.1.6](https://github.com/leo-project/leo_commons/releases/tag/1.1.6)
    * [leo_dcerl v0.4.1](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.1)
    * [leo_logger v1.2.2](https://github.com/leo-project/leo_logger/releases/tag/1.2.2)
    * [leo_mcerl v0.6.0](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.0)
    * [leo_mq v1.3.18](https://github.com/leo-project/leo_mq/releases/tag/1.3.18)
    * [leo_object_storage](https://github.com/leo-project/leo_object_storage/tag/1.2.12)
    * [leo_ordning_reda v1.1.4](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.1.4)
    * [leo_redundant_manager 1.9.22](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.22)
    * [leo_rpc v0.10.7](https://github.com/leo-project/leo_rpc/releases/tag/0.10.7)
    * [leo_pod v0.6.4](https://github.com/leo-project/leo_pod/releases/tag/0.6.4)
    * [leo_s3_libs v1.1.10](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.10)
    * [leo_statistics v1.1.10](https://github.com/leo-project/leo_statistics/releases/tag/1.1.10)
    * [leo_tran v0.2.4](https://github.com/leo-project/leo_tran/releases/tag/0.2.4)
    * [leo_watchdog v0.12.1](https://github.com/leo-project/leo_watchdog/releases/tag/0.12.1)
    * [savanna_agent v0.4.13](https://github.com/leo-project/savanna_agent/releases/tag/0.4.13)
    * [savanna_commons v0.8.16](https://github.com/leo-project/savanna_commons/releases/tag/0.8.16)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
    * [nfs_rpc_server v0.2.3](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.3)
    * [leo_gateway v1.2.18](https://github.com/leo-project/leo_gateway/releases/tag/1.2.18)
    * [leo_manager v1.2.18](https://github.com/leo-project/leo_manager/releases/tag/1.2.18)
    * [leo_storage v1.2.18](https://github.com/leo-project/leo_storage/releases/tag/1.2.18)
* others
    * [bitcask v2.0.1](https://github.com/basho/bitcask/releases/tag/2.0.1)
    * [cowboy v1.0.0](https://github.com/leo-project/cowboy/releases/tag/1.0.0-p1)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v2.1.4](https://github.com/basho/eleveldb/releases/tag/2.1.4)
    * [folsom v0.8.2-for-leofs](https://github.com/leo-project/folsom/releases/tag/0.8.2-for-leofs)
    * [jiffy v0.14.4](https://github.com/davisp/jiffy/releases/tag/0.14.4)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2)
    * [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.2.16
## Bugs Fixed

* [#422](https://github.com/leo-project/leofs/issues/422) ``leo_storage`` A LeoStorage node crashed due to executing consecutive the "ls" command
* [#423](https://github.com/leo-project/leofs/issues/423)``v1.2.11-`` ``leo_watchdog`` Did not clear combination of for errors - "cpu_util" and "load_avg_1m"

## Used Libraries

* leo project
    * [leo_backend-db v1.1.13](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.13)
    * [leo_cache v0.6.5](https://github.com/leo-project/leo_cache/releases/tag/0.6.5)
    * [leo_commons v1.1.5](https://github.com/leo-project/leo_commons/releases/tag/1.1.4)
    * [leo_dcerl v0.4.0](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.0)
    * [leo_logger v1.1.11](https://github.com/leo-project/leo_logger/releases/tag/1.1.11)
    * [leo_mcerl v0.6.0](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.0)
    * [leo_mq v1.3.16](https://github.com/leo-project/leo_mq/releases/tag/1.3.16)
    * [leo_object_storage v1.2.10](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.10)
    * [leo_ordning_reda v1.1.3](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.1.3)
    * [leo_redundant_manager 1.9.19](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.19)
    * [leo_rpc v0.10.6](https://github.com/leo-project/leo_rpc/releases/tag/0.10.6)
    * [leo_pod v0.6.6](https://github.com/leo-project/leo_pod/releases/tag/0.6.6)
    * [leo_s3_libs v1.1.9](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.9)
    * [leo_statistics v1.1.9](https://github.com/leo-project/leo_statistics/releases/tag/1.1.9)
    * [leo_watchdog v0.12.0](https://github.com/leo-project/leo_watchdog/releases/tag/0.12.0)
    * [savanna_agent v0.4.12](https://github.com/leo-project/savanna_agent/releases/tag/0.4.12)
    * [savanna_commons v0.8.15](https://github.com/leo-project/savanna_commons/releases/tag/0.8.15)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
    * [nfs_rpc_server v0.2.3](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.3)
    * [leo_gateway v1.2.16](https://github.com/leo-project/leo_gateway/releases/tag/1.2.16)
    * [leo_manager v1.2.16](https://github.com/leo-project/leo_manager/releases/tag/1.2.16)
    * [leo_storage v1.2.16](https://github.com/leo-project/leo_storage/releases/tag/1.2.16)
* others
    * [bitcask v2.0.0](https://github.com/basho/bitcask/releases/tag/2.0.0)
    * [cowboy v1.0.1](https://github.com/leo-project/cowboy/releases/tag/for-leofs-1.2.11)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v2.1.1](https://github.com/basho/eleveldb/releases/tag/2.1.1)
    * [folsom v0.8.2-for-leofs](https://github.com/leo-project/folsom/releases/tag/0.8.2-for-leofs)
    * [jiffy v0.13.3](https://github.com/davisp/jiffy/releases/tag/0.13.3)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2)
    * [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.2.14
## Bugs Fixed

* [#416](https://github.com/leo-project/leofs/issues/416) ``leo_manager`` ``leo_redundant_manager`` Cannot execute the rebalance command with manual operaiton
    * It was happened while one manager-node downed
* [#417](https://github.com/leo-project/leofs/issues/417) ``leo_manager`` ``leo_redundant_manager`` It possible of inconsistent status of a storage-node between leo_redundant_manager and leo_manager
* [#418](https://github.com/leo-project/leofs/issues/418) ``leo_manager`` leo_manager slave can't restart when detach was executed while the slave was down


## Used Libraries

* leo project
    * [leo_backend-db v1.1.9](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.9)
    * [leo_cache v0.6.5](https://github.com/leo-project/leo_cache/releases/tag/0.6.5)
    * [leo_commons v1.1.4](https://github.com/leo-project/leo_commons/releases/tag/1.1.4)
    * [leo_dcerl v0.4.0](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.0)
    * [leo_logger v1.1.6](https://github.com/leo-project/leo_logger/releases/tag/1.1.6)
    * [leo_mcerl v0.6.0](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.0)
    * [leo_mq v1.3.14](https://github.com/leo-project/leo_mq/releases/tag/1.3.14)
    * [leo_object_storage v1.2.9](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.9)
    * [leo_ordning_reda v1.1.0](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.1.0)
    * [leo_redundant_manager 1.9.18](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.18)
    * [leo_rpc v0.10.5](https://github.com/leo-project/leo_rpc/releases/tag/0.10.5)
    * [leo_pod v0.6.6](https://github.com/leo-project/leo_pod/releases/tag/0.6.6)
    * [leo_s3_libs v1.1.8](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.8)
    * [leo_statistics v1.1.6](https://github.com/leo-project/leo_statistics/releases/tag/1.1.6)
    * [leo_watchdog v0.10.2](https://github.com/leo-project/leo_watchdog/releases/tag/0.10.2)
    * [savanna_agent v0.4.9](https://github.com/leo-project/savanna_agent/releases/tag/0.4.9)
    * [savanna_commons v0.8.12](https://github.com/leo-project/savanna_commons/releases/tag/0.8.12)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
    * [nfs_rpc_server v0.2.3](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.3)
    * [leo_gateway v1.2.14](https://github.com/leo-project/leo_gateway/releases/tag/1.2.14)
    * [leo_manager v1.2.14](https://github.com/leo-project/leo_manager/releases/tag/1.2.14)
    * [leo_storage v1.2.14](https://github.com/leo-project/leo_storage/releases/tag/1.2.14)
* others
    * [bitcask v2.0.0](https://github.com/basho/bitcask/releases/tag/2.0.0)
    * [cowboy v1.0.1](https://github.com/leo-project/cowboy/releases/tag/for-leofs-1.2.11)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v2.1.1](https://github.com/basho/eleveldb/releases/tag/2.1.1)
    * [folsom v0.8.2-for-leofs](https://github.com/leo-project/folsom/releases/tag/0.8.2-for-leofs)
    * [jiffy v0.13.3](https://github.com/davisp/jiffy/releases/tag/0.13.3)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2)
    * [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.2.12
## Features and Improvements

* Able to use ``compact-suspend`` comamnd when the auto-compaction is enabled

## Bugs Fixed

* [#390](https://github.com/leo-project/leofs/issues/390) ``leo_object_storage`` Unexpected 500 error when getting a zero size object not cached
* [#393](https://github.com/leo-project/leofs/issues/393) ``leo_object_storage`` Compaction can be slow because of the watchdog communication
* [#395](https://github.com/leo-project/leofs/issues/395) ``leo_gateway`` ``s3tests.functional.test_s3:test_bucket_list_maxkeys_zero`` failed
* [#396](https://github.com/leo-project/leofs/issues/396) ``leo_storage`` Auto Compaction tend to be executed on the same node
* [#398](https://github.com/leo-project/leofs/issues/398) ``leo_object_storage`` Compaction may stop unintentionally under heavy load

## Used Libraries

* leo project
    * [leo_backend-db v1.1.9](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.9)
    * [leo_cache v0.6.5](https://github.com/leo-project/leo_cache/releases/tag/0.6.5)
    * [leo_commons v1.1.2](https://github.com/leo-project/leo_commons/releases/tag/1.1.2)
    * [leo_dcerl v0.4.0](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.0)
    * [leo_logger v1.1.6](https://github.com/leo-project/leo_logger/releases/tag/1.1.6)
    * [leo_mcerl v0.6.0](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.0)
    * [leo_mq v1.3.11](https://github.com/leo-project/leo_mq/releases/tag/1.3.11)
    * [leo_object_storage v1.2.9](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.9)
    * [leo_ordning_reda v1.1.0](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.1.0)
    * [leo_redundant_manager 1.9.15](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.15)
    * [leo_rpc v0.10.2](https://github.com/leo-project/leo_rpc/releases/tag/0.10.2)
    * [leo_pod v0.6.4](https://github.com/leo-project/leo_pod/releases/tag/0.6.4)
    * [leo_s3_libs v1.1.8](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.8)
    * [leo_statistics v1.1.6](https://github.com/leo-project/leo_statistics/releases/tag/1.1.6)
    * [leo_watchdog v0.10.2](https://github.com/leo-project/leo_watchdog/releases/tag/0.10.2)
    * [savanna_agent v0.4.9](https://github.com/leo-project/savanna_agent/releases/tag/0.4.9)
    * [savanna_commons v0.8.12](https://github.com/leo-project/savanna_commons/releases/tag/0.8.12)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
    * [nfs_rpc_server v0.2.3](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.3)
    * [leo_gateway v1.2.12](https://github.com/leo-project/leo_gateway/releases/tag/1.2.12)
    * [leo_manager v1.2.12](https://github.com/leo-project/leo_manager/releases/tag/1.2.12)
    * [leo_storage v1.2.12](https://github.com/leo-project/leo_storage/releases/tag/1.2.12)
* others
    * [bitcask v2.0.0](https://github.com/basho/bitcask/releases/tag/2.0.0)
    * [cowboy v1.0.1](https://github.com/leo-project/cowboy/releases/tag/for-leofs-1.2.11)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v2.1.1](https://github.com/basho/eleveldb/releases/tag/2.1.1)
    * [folsom v0.8.2-for-leofs](https://github.com/leo-project/folsom/releases/tag/0.8.2-for-leofs)
    * [jiffy v0.13.3](https://github.com/davisp/jiffy/releases/tag/0.13.3)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2)
    * [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.2.11
## Features and Improvements

*  [#377](https://github.com/leo-project/leofs/issues/377) ``leo_gateway`` Support logging bucket operation
*  ``leofs-adm`` Added ``diagnose-start`` command

## Bugs Fixed

*  Retrieves range of an object
    *  [#376](https://github.com/leo-project/leofs/issues/376) ``leo_gateway`` Could not get an object with range
    *  [#382](https://github.com/leo-project/leofs/issues/382) ``leo_storage`` Incorrect handling of range get the first byte
* Leo Storage
    *  [#365](https://github.com/leo-project/leofs/issues/365) ``leo_storage`` ``auto-compaction`` Each storage node's data-compaction happened at almost same time
    *  [#378](https://github.com/leo-project/leofs/issues/378) ``leo_object_storage`` file:pread/3 could return {ok, Data} which size less than the number argument
    *  [#379](https://github.com/leo-project/leofs/issues/379) ``leo_storage`` ``leo_mq`` Decreased a performance of read and write operation during rebalance/recover-node
    *  [#383](https://github.com/leo-project/leofs/issues/383) ``leo_object_storage`` The result of du could be wrong
    *  [#385](https://github.com/leo-project/leofs/issues/385) ``leo_object_storage`` Found incorrect some data-blocks during the data-compaction
    *  [#387](https://github.com/leo-project/leofs/issues/387) ``leo_ordning_reda`` Close-operation should be executed when unexpected terminating
    *  Revised [leo_storage's configuration](https://github.com/leo-project/leo_storage/blob/master/priv/leo_storage.conf):
        *  Removed configurations to reduce administration costs and ``leo_storage`` calcurates each min-value and step-value
            *  compaction.waiting_time_min
            *  compaction.waiting_time_step
            *  compaction.batch_procs_min
            *  compaction.batch_procs_step
            *  mq.num_of_batch_process_min
            *  mq.num_of_batch_process_step
            *  mq.interval_between_batch_procs_min
            *  mq.interval_between_batch_procs_step
* Leo Gateway
    *  [#171](https://github.com/leo-project/leofs/issues/171) ``leo_gateway`` PHP client's headObject was not working
    *  [#384](https://github.com/leo-project/leofs/issues/384) ``leo_gateway`` Some access logs have been not recorded
* Leo Manager
    *  [#371](https://github.com/leo-project/leofs/issues/371) ``leo_manager`` ``leo_statistics`` Could NOT migrate from v1.0.x to 1.2

## Used Libraries

* leo project
    * [leo_backend-db v1.1.9](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.9)
    * [leo_cache v0.6.5](https://github.com/leo-project/leo_cache/releases/tag/0.6.5)
    * [leo_commons v1.1.2](https://github.com/leo-project/leo_commons/releases/tag/1.1.2)
    * [leo_dcerl v0.4.0](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.0)
    * [leo_logger v1.1.6](https://github.com/leo-project/leo_logger/releases/tag/1.1.6)
    * [leo_mcerl v0.6.0](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.0)
    * [leo_mq v1.3.11](https://github.com/leo-project/leo_mq/releases/tag/1.3.11)
    * [leo_object_storage v1.2.8](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.8)
    * [leo_ordning_reda v1.1.0](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.1.0)
    * [leo_redundant_manager 1.9.15](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.15)
    * [leo_rpc v0.10.2](https://github.com/leo-project/leo_rpc/releases/tag/0.10.2)
    * [leo_pod v0.6.4](https://github.com/leo-project/leo_pod/releases/tag/0.6.4)
    * [leo_s3_libs v1.1.8](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.8)
    * [leo_statistics v1.1.6](https://github.com/leo-project/leo_statistics/releases/tag/1.1.6)
    * [leo_watchdog v0.10.1](https://github.com/leo-project/leo_watchdog/releases/tag/0.10.1)
    * [savanna_agent v0.4.9](https://github.com/leo-project/savanna_agent/releases/tag/0.4.9)
    * [savanna_commons v0.8.12](https://github.com/leo-project/savanna_commons/releases/tag/0.8.12)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
    * [nfs_rpc_server v0.2.3](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.3)
    * [leo_gateway v1.2.11](https://github.com/leo-project/leo_gateway/releases/tag/1.2.11)
    * [leo_manager v1.2.11](https://github.com/leo-project/leo_manager/releases/tag/1.2.11)
    * [leo_storage v1.2.11](https://github.com/leo-project/leo_storage/releases/tag/1.2.11)
* others
    * [bitcask v2.0.0](https://github.com/basho/bitcask/releases/tag/2.0.0)
    * [cowboy v1.0.0](https://github.com/leo-project/cowboy/releases/tag/1.0.0)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v2.1.1](https://github.com/basho/eleveldb/releases/tag/2.1.1)
    * [folsom v0.8.2-for-leofs](https://github.com/leo-project/folsom/releases/tag/0.8.2-for-leofs)
    * [jiffy v0.13.3](https://github.com/davisp/jiffy/releases/tag/0.13.3)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2)
    * [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.2.10
## Bugs Fixed

* [#366](https://github.com/leo-project/leofs/issues/366) ``leo_storage`` Cannot migrate old one to LeoFS v1.2.9
    * We're planning to implement and provide the auto-migration-test tool
* ``leo_statistics`` Strictly check creating mnesia's tables

## Used Libraries

* leo project
    * [leo_backend-db v1.1.8](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.8)
    * [leo_cache v0.6.3](https://github.com/leo-project/leo_cache/releases/tag/0.6.3)
    * [leo_commons v1.1.1](https://github.com/leo-project/leo_commons/releases/tag/1.1.1)
    * [leo_dcerl v0.2.12](https://github.com/leo-project/leo_dcerl/releases/tag/0.2.12)
    * [leo_logger v1.1.5](https://github.com/leo-project/leo_logger/releases/tag/1.1.5)
    * [leo_mcerl v0.4.1](https://github.com/leo-project/leo_mcerl/releases/tag/0.4.1)
    * [leo_mq v1.3.9](https://github.com/leo-project/leo_mq/releases/tag/1.3.9)
    * [leo_object_storage v1.2.7](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.7)
    * [leo_ordning_reda v1.0.0](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.0.0)
    * [leo_redundant_manager 1.9.13](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.13)
    * [leo_rpc v0.10.1](https://github.com/leo-project/leo_rpc/releases/tag/0.10.1)
    * [leo_pod v0.6.3](https://github.com/leo-project/leo_pod/releases/tag/0.6.3)
    * [leo_s3_libs v1.1.7](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.7)
    * [leo_statistics v1.1.4](https://github.com/leo-project/leo_statistics/releases/tag/1.1.4)
    * [leo_watchdog v0.8.0](https://github.com/leo-project/leo_watchdog/releases/tag/0.8.0)
    * [savanna_agent v0.4.7](https://github.com/leo-project/savanna_agent/releases/tag/0.4.7)
    * [savanna_commons v0.8.10](https://github.com/leo-project/savanna_commons/releases/tag/0.8.10)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
    * [nfs_rpc_server v0.2.3](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.3)
    * [leo_gateway v1.2.10](https://github.com/leo-project/leo_gateway/releases/tag/1.2.10)
    * [leo_manager v1.2.10](https://github.com/leo-project/leo_manager/releases/tag/1.2.10)
    * [leo_storage v1.2.10](https://github.com/leo-project/leo_storage/releases/tag/1.2.10)
* others
    * [bitcask v2.0.0](https://github.com/basho/bitcask/releases/tag/2.0.0)
    * [cowboy v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb/releases/tag/1.4.10)
    * [folsom v0.8.1](https://github.com/boundary/folsom/releases/tag/0.8.1)
    * [jiffy v0.13.3](https://github.com/davisp/jiffy/releases/tag/0.13.3)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)
    * [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.2.9
## Features and Improvements

* Improvements
    * [#354](https://github.com/leo-project/leofs/issues/354) ``leo_gateway`` ``leo_storage`` Exit when the number of retries reached a certain time
    * [#356](https://github.com/leo-project/leofs/issues/356) ``leo_object_storage`` Implemented output of a low I/O operation's log
    * [#358](https://github.com/leo-project/leofs/issues/358) ``leo_watchdog`` ``leo_manager`` Implemented changing configuration by leo_manager's console
    * [#360](https://github.com/leo-project/leofs/issues/360) ``leo_gateway`` Replace ``erlang:now`` with ``os:timestamp`` to improve the performance

## Bugs Fixed

* Improve performance of recover-node and rebalance processing
    * [#359](https://github.com/leo-project/leofs/issues/359) ``leo_mq`` ``leo_storage`` CPU can be bottleneck in case doing ``rebalance/recover``
    * [#362](https://github.com/leo-project/leofs/issues/362) ``leo_backend_db`` ``leo_backend_db_bitcask:first/1`` function can use cpu more necessary
    * [Benchmark LeoFS v1.2.9 w/recover-node, watchdog and auto-compaction](https://github.com/leo-project/notes/tree/master/leofs/benchmark/leofs/recover_with_front/20150515_recover_node_1.2.9)
* [#361](https://github.com/leo-project/leofs/issues/361) ``leo_statistics`` ``leo_storage`` ``leo_gateway`` ``leo_manager`` All Leo's servers can start normally even if starting statistics related erlang processes failed

## Used Libraries

* leo project
    * [leo_backend-db v1.1.7](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.7)
    * [leo_cache v0.6.3](https://github.com/leo-project/leo_cache/releases/tag/0.6.3)
    * [leo_commons v1.1.1](https://github.com/leo-project/leo_commons/releases/tag/1.1.1)
    * [leo_dcerl v0.2.12](https://github.com/leo-project/leo_dcerl/releases/tag/0.2.12)
    * [leo_logger v1.1.5](https://github.com/leo-project/leo_logger/releases/tag/1.1.5)
    * [leo_mcerl v0.4.1](https://github.com/leo-project/leo_mcerl/releases/tag/0.4.1)
    * [leo_mq v1.3.8](https://github.com/leo-project/leo_mq/releases/tag/1.3.8)
    * [leo_object_storage v1.2.6](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.6)
    * [leo_ordning_reda v1.0.0](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.0.0)
    * [leo_redundant_manager 1.9.12](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.12)
    * [leo_rpc v0.10.1](https://github.com/leo-project/leo_rpc/releases/tag/0.10.1)
    * [leo_pod v0.6.3](https://github.com/leo-project/leo_pod/releases/tag/0.6.3)
    * [leo_s3_libs v1.1.7](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.7)
    * [leo_statistics v1.1.3](https://github.com/leo-project/leo_statistics/releases/tag/1.1.3)
    * [leo_watchdog v0.8.0](https://github.com/leo-project/leo_watchdog/releases/tag/0.8.0)
    * [savanna_agent v0.4.6](https://github.com/leo-project/savanna_agent/releases/tag/0.4.6)
    * [savanna_commons v0.8.9](https://github.com/leo-project/savanna_commons/releases/tag/0.8.9)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
    * [nfs_rpc_server v0.2.3](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.3)
    * [leo_gateway v1.2.9](https://github.com/leo-project/leo_gateway/releases/tag/1.2.9)
    * [leo_manager v1.2.9](https://github.com/leo-project/leo_manager/releases/tag/1.2.9)
    * [leo_storage v1.2.9](https://github.com/leo-project/leo_storage/releases/tag/1.2.9)
* others
    * [bitcask v2.0.0](https://github.com/basho/bitcask/releases/tag/2.0.0)
    * [cowboy v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb/releases/tag/1.4.10)
    * [folsom v0.8.1](https://github.com/boundary/folsom/releases/tag/0.8.1)
    * [jiffy v0.13.3](https://github.com/davisp/jiffy/releases/tag/0.13.3)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)
    * [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.2.8
## Features and Improvements

* Improvements
    * [#347](https://github.com/leo-project/leofs/issues/347) Delete Multiple Objects method of S3 API support
    * Implemented the mq-stats command for JSON format

## Bugs Fixed

* Multi Data Center related issues
    * [#329](https://github.com/leo-project/leofs/issues/329) ``mdc-replication`` Wrong cluster-status when executing the command on a remote-cluster's manager
    * [#343](https://github.com/leo-project/leofs/issues/343) ``leo_storage`` ``mdc`` Replication messages coudn't be consumed at all
    * [#345](https://github.com/leo-project/leofs/issues/345) ``leo_storage`` ``mdc`` Replication DELETE messages coudn't be consumed properly when the target objects already have been compacted
* Storage related issues
    * [#333](https://github.com/leo-project/leofs/issues/333) ``leo_storage`` "case_clause" could happen when the target object was already deleted
    * [#344](https://github.com/leo-project/leofs/issues/344) ``leo_storage`` Refactor leo_storage_handler_object to reduce complexity
* Gateway related issues
    * [#328](https://github.com/leo-project/leofs/issues/328) ``leo_gateway`` Cannot stop gateway with leo_gateway script
        * [#340](https://github.com/leo-project/leofs/issues/340) ``leo_gateway`` leo_gateway can't stop (kill -0) properly on OSX
    * [#348](https://github.com/leo-project/leofs/issues/348) ``leo_gateway`` Files uploaded with its name including urlencoded chars stored improper name
    * [#349](https://github.com/leo-project/leofs/issues/349) ``leo_gateway`` Get object with range header returns empty(wrong) content
* Othe issues
    * [#141](https://github.com/leo-project/leofs/issues/141) ``leo_rpc``Performance degradation due to leaking tcp connections

## Used Libraries

* leo project
    * [leo_backend-db v1.1.6](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.6)
    * [leo_cache v0.6.3](https://github.com/leo-project/leo_cache/releases/tag/0.6.3)
    * [leo_commons v1.1.1](https://github.com/leo-project/leo_commons/releases/tag/1.1.1)
    * [leo_dcerl v0.2.12](https://github.com/leo-project/leo_dcerl/releases/tag/0.2.12)
    * [leo_logger v1.1.5](https://github.com/leo-project/leo_logger/releases/tag/1.1.5)
    * [leo_mcerl v0.4.1](https://github.com/leo-project/leo_mcerl/releases/tag/0.4.1)
    * [leo_mq v1.3.6](https://github.com/leo-project/leo_mq/releases/tag/1.3.6)
    * [leo_object_storage v1.2.5](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.5)
    * [leo_ordning_reda v1.0.0](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.0.0)
    * [leo_redundant_manager 1.9.10](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.10)
    * [leo_rpc v0.10.1](https://github.com/leo-project/leo_rpc/releases/tag/0.10.1)
    * [leo_pod v0.6.3](https://github.com/leo-project/leo_pod/releases/tag/0.6.3)
    * [leo_s3_libs v1.1.7](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.7)
    * [leo_statistics v1.1.2](https://github.com/leo-project/leo_statistics/releases/tag/1.1.2)
    * [leo_watchdog v0.6.4](https://github.com/leo-project/leo_watchdog/releases/tag/0.6.4)
    * [savanna_agent v0.4.6](https://github.com/leo-project/savanna_agent/releases/tag/0.4.6)
    * [savanna_commons v0.8.9](https://github.com/leo-project/savanna_commons/releases/tag/0.8.9)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
    * [nfs_rpc_server v0.2.3](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.3)
    * [leo_gateway v1.2.8](https://github.com/leo-project/leo_gateway/releases/tag/1.2.8)
    * [leo_manager v1.2.8](https://github.com/leo-project/leo_manager/releases/tag/1.2.8)
    * [leo_storage v1.2.8](https://github.com/leo-project/leo_storage/releases/tag/1.2.8)
* others
    * [bitcask v1.7.1](https://github.com/leo-project/bitcask/releases/tag/1.7.1)
    * [cowboy v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb/releases/tag/1.4.10)
    * [folsom v0.8.1](https://github.com/boundary/folsom/releases/tag/0.8.1)
    * [jiffy v0.13.3](https://github.com/davisp/jiffy/releases/tag/0.13.3)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)
    * [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.2.7
## Features and Improvements

* [#320](https://github.com/leo-project/leofs/issues/320) ``leo_mq`` Can take too much time to get started consuming messages in a MQ

## Bugs Fixed

* [#313](https://github.com/leo-project/leofs/issues/313) ``leo_watchdog`` Watchdog downed every an hour
* [#315](https://github.com/leo-project/leofs/issues/315) ``leo_redundant_manager`` Inconsistent storage node status
* [#317](https://github.com/leo-project/leofs/issues/317) ``leo_redundant_manager`` Calling leo_manager_api:get_members_of_all_versions get timeout on leo_gateway
* [#319](https://github.com/leo-project/leofs/issues/319) ``leo_gateway`` List Objects with ``next-marker`` could NOT work when using ``s3cmd`` as a client
* [#322](https://github.com/leo-project/leofs/issues/322) ``leo_cache`` Set wrong value to a disk cache capacity
* [#323](https://github.com/leo-project/leofs/issues/323) ``leo_dcerl`` LeoFS's gateway do not response or return HTTP error code 5xx

## Used Libraries

* leo project
    * [leo_backend-db v1.1.5](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.5)
    * [leo_cache v0.6.3](https://github.com/leo-project/leo_cache/releases/tag/0.6.3)
    * [leo_commons v1.1.1](https://github.com/leo-project/leo_commons/releases/tag/1.1.1)
    * [leo_dcerl v0.2.12](https://github.com/leo-project/leo_dcerl/releases/tag/0.2.12)
    * [leo_logger v1.1.5](https://github.com/leo-project/leo_logger/releases/tag/1.1.5)
    * [leo_mcerl v0.4.1](https://github.com/leo-project/leo_mcerl/releases/tag/0.4.1)
    * [leo_mq v1.3.4](https://github.com/leo-project/leo_mq/releases/tag/1.3.4)
    * [leo_object_storage v1.2.4](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.4)
    * [leo_ordning_reda v0.10.10](https://github.com/leo-project/leo_ordning_reda/releases/tag/0.10.10)
    * [leo_redundant_manager 1.9.8](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.8)
    * [leo_rpc v0.10.0](https://github.com/leo-project/leo_rpc/releases/tag/0.10.0)
    * [leo_pod v0.6.2](https://github.com/leo-project/leo_pod/releases/tag/0.6.2)
    * [leo_s3_libs v1.1.6](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.6)
    * [leo_statistics v1.1.0](https://github.com/leo-project/leo_statistics/releases/tag/1.1.0)
    * [leo_watchdog v0.6.3](https://github.com/leo-project/leo_watchdog/releases/tag/0.6.3)
    * [savanna_agent v0.4.5](https://github.com/leo-project/savanna_agentreleases/tag/0.4.5)
    * [savanna_commons v0.8.7](https://github.com/leo-project/savanna_commons/releases/tag/0.8.7)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
    * [nfs_rpc_server v0.2.3](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.3)
    * [leo_gateway v1.2.7](https://github.com/leo-project/leo_gateway/releases/tag/1.2.7)
    * [leo_manager v1.2.7](https://github.com/leo-project/leo_manager/releases/tag/1.2.7)
    * [leo_storage v1.2.7](https://github.com/leo-project/leo_storage/releases/tag/1.2.7)
* others
    * [bitcask v1.7.1](https://github.com/leo-project/bitcask/releases/tag/1.7.1)
    * [cowboy v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb/releases/tag/1.4.10)
    * [folsom v0.8.1](https://github.com/boundary/folsom/releases/tag/0.8.1)
    * [jiffy v0.13.3](https://github.com/davisp/jiffy/releases/tag/0.13.3)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)
    * [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.2.6
## Features and Improvements

* [#296](https://github.com/leo-project/leofs/issues/296) ``leo_commons`` Replace leo_hashtable with Erlang built-in function
* [#305](https://github.com/leo-project/leofs/issues/305) ``leo_statistics`` some system metrics should be added for more reliability

## Bugs Fixed

* [#307](https://github.com/leo-project/leofs/issues/307) ``leo_storage`` Could not respond not existed object immediately (LeoFS v1.2.5's bug)
    * LeoFS Gateway was timeout because LeoFS Storage could not respond not existed object immediately.
And also, load of a primary of storage-node became high by this issue.
    * We took measure for this situation we implemented [leofs_test2](https://github.com/leo-project/leofs_test2) which is an integration test tool.
* [#308](https://github.com/leo-project/leofs/issues/308) ``leo_storage`` ``leo_gateway`` Could not synchronize 'suspend' status at each gateway and storage nodes
* [#309](https://github.com/leo-project/leofs/issues/309) ``leo_redundant_manager`` ``leo_manager`` Ring status to be synced between mnesia and worker state
* [#310](https://github.com/leo-project/leofs/issues/310) ``leo_gateway`` Delete request might consume CPU resource more than necessary
* [#311](https://github.com/leo-project/leofs/issues/311) ``leo_storage`` Could NOT respond an object when facing the inconsistent object

## Used Libraries

* leo project
    * [leo_backend-db v1.1.4](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.4)
    * [leo_cache v0.6.0](https://github.com/leo-project/leo_cache/releases/tag/0.6.0)
    * [leo_commons v1.1.0](https://github.com/leo-project/leo_commons/releases/tag/1.1.0)
    * [leo_dcerl v0.2.11](https://github.com/leo-project/leo_dcerl/releases/tag/0.2.11)
    * [leo_logger v1.1.4](https://github.com/leo-project/leo_logger/releases/tag/1.1.4)
    * [leo_mcerl v0.4.1](https://github.com/leo-project/leo_mcerl/releases/tag/0.4.1)
    * [leo_mq v1.3.3](https://github.com/leo-project/leo_mq/releases/tag/1.3.3)
    * [leo_object_storage v1.2.3](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.3)
    * [leo_ordning_reda v0.10.9](https://github.com/leo-project/leo_ordning_reda/releases/tag/0.10.9)
    * [leo_redundant_manager 1.9.7](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.7)
    * [leo_rpc v0.8.10](https://github.com/leo-project/leo_rpc/releases/tag/0.8.10)
    * [leo_pod v0.6.2](https://github.com/leo-project/leo_pod/releases/tag/0.6.2)
    * [leo_s3_libs v1.1.5](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.5)
    * [leo_statistics v1.0.9](https://github.com/leo-project/leo_statistics/releases/tag/1.0.9)
    * [leo_watchdog v0.6.2](https://github.com/leo-project/leo_watchdog/releases/tag/0.6.2)
    * [savanna_agent v0.4.4](https://github.com/leo-project/savanna_agentreleases/tag/0.4.4)
    * [savanna_commons v0.8.6](https://github.com/leo-project/savanna_commons/releases/tag/0.8.6)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
    * [nfs_rpc_server v0.2.2](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.2)
    * [leo_gateway v1.2.6](https://github.com/leo-project/leo_gateway/releases/tag/1.2.6)
    * [leo_manager v1.2.6](https://github.com/leo-project/leo_manager/releases/tag/1.2.6)
    * [leo_storage v1.2.6](https://github.com/leo-project/leo_storage/releases/tag/1.2.6)
* others
    * [bitcask v1.7.1](https://github.com/leo-project/bitcask/releases/tag/1.7.1)
    * [cowboy v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb/releases/tag/1.4.10)
    * [folsom v0.8.1](https://github.com/boundary/folsom/releases/tag/0.8.1)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy/releases/tag/0.2.2)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)
    * [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.2.5
## Features and Improvements

* Started to provide LeoFS integration test tool - [leofs_test](https://github.com/leo-project/leofs_test2)
    * We're able to easily check LeoFS with it whether latest LeoFS has issues or not before getting installed LeoFS in your dev/staging/production environment(s).


## Bugs Fixed

* [#299](https://github.com/leo-project/leofs/issues/299) ``leo_redundant_manager`` Inconsistent ring of a resumed storage-node which is timing issue
* [#301](https://github.com/leo-project/leofs/issues/301) ``leo_storage`` Replication messages could be lost in some edge cases
* ``leo_storage`` Fix the read-and-repair mechanism. Some inconsistent data could not recover with it.


## Used Libraries

* leo project
    * [leo_backend-db v1.1.4](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.4)
    * [leo_cache v0.6.0](https://github.com/leo-project/leo_cache/releases/tag/0.6.0)
    * [leo_commons v1.1.0](https://github.com/leo-project/leo_commons/releases/tag/1.1.0)
    * [leo_dcerl v0.2.11](https://github.com/leo-project/leo_dcerl/releases/tag/0.2.11)
    * [leo_logger v1.1.4](https://github.com/leo-project/leo_logger/releases/tag/1.1.4)
    * [leo_mcerl v0.4.1](https://github.com/leo-project/leo_mcerl/releases/tag/0.4.1)
    * [leo_mq v1.3.3](https://github.com/leo-project/leo_mq/releases/tag/1.3.3)
    * [leo_object_storage v1.2.3](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.3)
    * [leo_ordning_reda v0.10.9](https://github.com/leo-project/leo_ordning_reda/releases/tag/0.10.9)
    * [leo_redundant_manager 1.9.6](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.6)
    * [leo_rpc v0.8.10](https://github.com/leo-project/leo_rpc/releases/tag/0.8.10)
    * [leo_pod v0.6.2](https://github.com/leo-project/leo_pod/releases/tag/0.6.2)
    * [leo_s3_libs v1.1.5](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.5)
    * [leo_statistics v1.0.8](https://github.com/leo-project/leo_statistics/releases/tag/1.0.8)
    * [leo_watchdog v0.6.2](https://github.com/leo-project/leo_watchdog/releases/tag/0.6.2)
    * [savanna_agent v0.4.4](https://github.com/leo-project/savanna_agentreleases/tag/0.4.4)
    * [savanna_commons v0.8.6](https://github.com/leo-project/savanna_commons/releases/tag/0.8.6)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
    * [nfs_rpc_server v0.2.2](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.2)
    * [leo_gateway v1.2.5](https://github.com/leo-project/leo_gateway/releases/tag/1.2.5)
    * [leo_manager v1.2.5](https://github.com/leo-project/leo_manager/releases/tag/1.2.5)
    * [leo_storage v1.2.5](https://github.com/leo-project/leo_storage/releases/tag/1.2.5)
* others
    * [bitcask v1.7.1](https://github.com/leo-project/bitcask/releases/tag/1.7.1)
    * [cowboy v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb/releases/tag/1.4.10)
    * [folsom v0.8.1](https://github.com/boundary/folsom/releases/tag/0.8.1)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy/releases/tag/0.2.2)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


# 1.2.4
## Bugs Fixed

* [#297](https://github.com/leo-project/leofs/issues/297) ``leo_object_storage`` Could NOT handle delete requests which made by the MQ mechanism at LeoFS v1.2.3

## Used Libraries

* leo project
    * [leo_backend-db v1.1.4](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.6.0](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.1.0](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.11](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.1.4](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.4.1](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.3.3](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.2.3](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.9](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager 1.9.5](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.10](https://github.com/leo-project/leo_rpc.git)
    * [leo_pod v0.6.2](https://github.com/leo-project/leo_pod.git)
    * [leo_s3_libs v1.1.5](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.8](https://github.com/leo-project/leo_statistics.git)
    * [leo_watchdog v0.6.1](https://github.com/leo-project/leo_watchdog.git)
    * [savanna_agent v0.4.4](https://github.com/leo-project/savanna_agent.git)
    * [savanna_commons v0.8.6](https://github.com/leo-project/savanna_commons.git)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen.git)
    * [nfs_rpc_server v0.2.2](https://github.com/leo-project/nfs_rpc_server.git)
    * [leo_gateway v1.2.4](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.2.4](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.2.4](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.7.1](https://github.com/leo-project/bitcask.git)
    * [cowboy v1.0.0](https://github.com/extend/cowboy.git)
    * [cowlib v1.0.0](https://github.com/extend/cowboy.git)
    * [elarm v0.3.0](https://github.com/leo-project/elarm.git)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb.git)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


# 1.2.3
## Features and Improvements

* Improvements
    * [#285](https://github.com/leo-project/leofs/issues/285) ``leo_storage`` Implemented "cluster-watchdog" in order to keep best condition of a cluster
    * [#286](https://github.com/leo-project/leofs/issues/286) ``leo_storage`` Unified name of configuration items for the mq and the compaction

## Bugs Fixed

* [#288](https://github.com/leo-project/leofs/issues/288) ``leo_manager`` Fixed wrong mix/max values when issueing [the status &lt;storage-node&gt; command](http://leo-project.net/leofs/docs/admin_guide/admin_guide_2.html#status-command) with ``leofs-adm command``
* [#290](https://github.com/leo-project/leofs/issues/290) [the du detail &lt;storage-node&gt; command](http://leo-project.net/leofs/docs/admin_guide/admin_guide_5.html#du-detail) with ``leofs-adm command`` had broken 1.2.2
* [#292](https://github.com/leo-project/leofs/issues/292) ``leo_storage`` [proc_lib:start_link](http://erlang.org/doc/man/proc_lib.html#spawn_link-1) must be used instead of [erlang:spawn](http://erlang.org/doc/man/erlang.html#spawn-1)
* [#293](https://github.com/leo-project/leofs/issues/293) Fixed that remain objects after finished data-compaction, situation of which is combination auto-compaction and deletion of a bucket

## Used Libraries

* leo project
    * [leo_backend-db v1.1.4](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.6.0](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.1.0](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.11](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.1.4](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.4.1](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.3.3](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.2.2](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.9](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager 1.9.5](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.10](https://github.com/leo-project/leo_rpc.git)
    * [leo_pod v0.6.2](https://github.com/leo-project/leo_pod.git)
    * [leo_s3_libs v1.1.5](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.8](https://github.com/leo-project/leo_statistics.git)
    * [leo_watchdog v0.6.1](https://github.com/leo-project/leo_watchdog.git)
    * [savanna_agent v0.4.4](https://github.com/leo-project/savanna_agent.git)
    * [savanna_commons v0.8.6](https://github.com/leo-project/savanna_commons.git)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen.git)
    * [nfs_rpc_server v0.2.2](https://github.com/leo-project/nfs_rpc_server.git)
    * [leo_gateway v1.2.3](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.2.3](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.2.3](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.7.1](https://github.com/leo-project/bitcask.git)
    * [cowboy v1.0.0](https://github.com/extend/cowboy.git)
    * [cowlib v1.0.0](https://github.com/extend/cowboy.git)
    * [elarm v0.3.0](https://github.com/leo-project/elarm.git)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb.git)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


# 1.2.2
## Features and Improvements

* New Features
    * [#117](https://github.com/leo-project/leofs/issues/117) ``leo_storage`` Implemented the auto-compaction mechanism
      * The auto-compaction configuration file for [LeoFS Storage](https://github.com/leo-project/leo_storage/blob/develop/priv/leo_storage.conf#L139-L152), name of which is ``leo_storage.conf``
      * The auto-compaction configuration is as follows:
      ```bash
      ## [compaction] enabled compaction? - default:false
      autonomic_op.compaction.is_enabled = true

      ## [compaction] number of parallel procs - default:1
      autonomic_op.compaction.parallel_procs = 1

      ## [compaction] warning ratio of active size - default:70%
      autonomic_op.compaction.warn_active_size_ratio = 70

      ## [compaction] threshold ratio of active size - default:60%
      autonomic_op.compaction.threshold_active_size_ratio = 60
      ```
* Improvements
    * [#270](https://github.com/leo-project/leofs/issues/270) Supported leo_watchdog for FreeBSD and SmartOS
    * [#281](https://github.com/leo-project/leofs/issues/281) ``leo_gateway`` Made timeout configurable for the get and the find_by_parent_dir
    * The watchdog mechnism to production status
      * The watchdog configuration file for [LeoFS Storage](https://github.com/leo-project/leo_storage/blob/develop/priv/leo_storage.conf#L63-L136) and [LeoFS Gateway](https://github.com/leo-project/leo_gateway/blob/develop/priv/leo_gateway.conf#L161-L206), name of which are ``leo_storage.conf`` and ``leo_gateway.conf``
      * The watchdog configuration for CPU and Disk as follows:
      ```bash
      ##  Watchdog.CPU
      ##
      ## Is cpu-watchdog enabled - default:false
      watchdog.cpu.is_enabled = true

      ## cpu - raised error times
      watchdog.cpu.raised_error_times = 3

      ## cpu - watch interval - default:5sec
      watchdog.cpu.interval = 5

      ## Threshold CPU load avg for 1min/5min - default:5.0
      watchdog.cpu.threshold_cpu_load_avg = 5.0

      ## Threshold CPU load util - default:100 = "100%"
      watchdog.cpu.threshold_cpu_util = 100

      ##  Watchdog.DISK
      ##
      ## Is disk-watchdog enabled - default:false
      watchdog.disk.is_enabled = true

      ## disk - raised error times
      watchdog.disk.raised_error_times = 3

      ## disk - watch interval - default:1sec
      watchdog.disk.interval = 1

      ## Threshold disk use% - defalut:85%
      watchdog.disk.threshold_disk_use = 85

      ## Threshold disk util% - defalut:100%
      watchdog.disk.threshold_disk_util = 100

      ## Threshold disk read kb/sec - defalut:262144(KB)
      watchdog.disk.threshold_disk_rkb = 262144

      ## Threshold disk write kb/sec - defalut:262144(KB)
      watchdog.disk.threshold_disk_wkb = 262144

      ## disk target devices for checking disk utilization
      watchdog.disk.target_devices = []
      ```
      * Improved MQ's confurations for LeoFS Storage in order to be able to control system load
      ```bash
      ## [Number of bach processes of message]
      ##
      ## Minimum number of bach processes of message
      mq.num_of_batch_process_min = 100

      ## Maxmim  number of bach processes of message
      mq.num_of_batch_process_max = 10000

      ## Maxmim  number of bach processes of message
      mq.num_of_batch_process_regular = 1000

      ## Maxmim  number of bach processes of message
      mq.num_of_batch_process_step = 250

      ## [Interval beween batch-procs]
      ##
      ## Minimum value of interval beween batch-procs(msec)
      mq.interval_between_batch_procs_min = 10

      ## Maximum value of interval between batch-procs(msec)
      mq.interval_between_batch_procs_max = 1000

      ## Regular value of interval between batch-procs(msec)
      mq.interval_between_batch_procs_regular = 300

      ## Step value of interval between batch-procs(msec)
      mq.interval_between_batch_procs_step = 100
      ```
      * Improved the ``mq-status`` command of ``leofs-adm``
      ```bash
      $ leofs-adm mq-stats storage_1@127.0.0.1
                    id                |    state    | number of msgs | batch of msgs  |    interval    |            description
      --------------------------------+-------------+----------------|----------------|----------------|-----------------------------------
      leo_delete_dir_queue            |   idling    | 0              | 1000           | 100            | delete directories
      leo_comp_meta_with_dc_queue     |   idling    | 0              | 1000           | 100            | compare metadata w/remote-node
      leo_sync_obj_with_dc_queue      |   idling    | 0              | 1000           | 100            | sync objs w/remote-node
      leo_recovery_node_queue         |   idling    | 0              | 1000           | 100            | recovery objs of node
      leo_async_deletion_queue        |   idling    | 0              | 1000           | 100            | async deletion of objs
      leo_rebalance_queue             |   running   | 2167           | 1400           | 10             | rebalance objs
      leo_sync_by_vnode_id_queue      |   idling    | 0              | 1000           | 100            | sync objs by vnode-id
      leo_per_object_queue            |   idling    | 0              | 1000           | 100            | recover inconsistent objs
       ```

## Bugs Fixed

* [#273](https://github.com/leo-project/leofs/issues/273) ``leo_gateway`` ``NFS`` Max file size was hardcoded as 4GB
* [#274](https://github.com/leo-project/leofs/issues/274) ``leo_manager`` ``leo_storage`` ``leo_gateway`` Crashed a manager node by snmp-related issue with Erlang-17
* [#275](https://github.com/leo-project/leofs/issues/275) ``leo_gateawy`` An error message was output if there is no the http custom header file
* [#277](https://github.com/leo-project/leofs/issues/277) ``leo_gateway`` In REST mode, Putting a large object failed
* [#279](https://github.com/leo-project/leofs/issues/279) ``leo_object_storage`` Compaction status can be inconsistent when an error occured while preparing compaction
* [#282](https://github.com/leo-project/leofs/issues/282) ``leo_gateway`` Gateway's timeout configuration has been ignored

## Used Libraries

* leo project
    * [leo_backend-db v1.1.4](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.6.0](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.1.0](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.11](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.1.4](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.4.1](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.3.0](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.2.0](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.9](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager 1.9.2](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.10](https://github.com/leo-project/leo_rpc.git)
    * [leo_pod v0.6.2](https://github.com/leo-project/leo_pod.git)
    * [leo_s3_libs v1.1.5](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.7](https://github.com/leo-project/leo_statistics.git)
    * [leo_watchdog v0.6.0](https://github.com/leo-project/leo_watchdog.git)
    * [savanna_agent v0.4.4](https://github.com/leo-project/savanna_agent.git)
    * [savanna_commons v0.8.6](https://github.com/leo-project/savanna_commons.git)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen.git)
    * [nfs_rpc_server v0.2.2](https://github.com/leo-project/nfs_rpc_server.git)
    * [leo_gateway v1.2.2](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.2.2](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.2.2](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.7.1](https://github.com/leo-project/bitcask.git)
    * [cowboy v1.0.0](https://github.com/extend/cowboy.git)
    * [cowlib v1.0.0](https://github.com/extend/cowboy.git)
    * [elarm v0.3.0](https://github.com/leo-project/elarm.git)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb.git)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


# 1.2.1
## Features and Improvements

* Improvements
    * Supported [LeoCenter](https://github.com/leo-project/leo_center) for LeoFS v1.2

## Bugs Fixed

* Fixed system migration issue from v1.x to v1.2.0

## Used Libraries

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


# 1.2.0
## Features and Improvements

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

## Bugs Fixed

* [#262](https://github.com/leo-project/leofs/issues/262) ``leo_gateway`` ``leo_storage`` Improved RPC in order to be able to handle huge traffic with large size objects
* [#263](https://github.com/leo-project/leofs/issues/263) ``leo_gateway`` Wrong error code when creating a bucket that already exists
* [#265](https://github.com/leo-project/leofs/issues/265) ``leo_object_storage`` Fixed that some succeeded updates might be ignored silently while executing compaction
* [#268](https://github.com/leo-project/leofs/issues/265) ``leo_object_storage`` Fixed that wrong output format of data-diagnosis when including children of an large-object

## Used Libraries

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
