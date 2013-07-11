CHANGELOG
=========

leofs-0.14.6 (July 11, 2013)
============================

* Improved
    * Able to set the max number of requests allowed in a single keep-alive session

* Bugs Fixed
    * Gateway
        * Fixed to respond HTTP-headers properly when using *disk-cache*
    * Manager
        * Fixed changed status from *running* to *restarted* when restarted leo-manager

* Used libraries
    * leo project
        * [leo_backend-db v0.12.17](https://github.com/leo-project/leo_backend_db.git)
        * [leo_cache v0.4.10](https://github.com/leo-project/leo_cache.git)
        * [leo_commons v0.14.4](https://github.com/leo-project/leo_commons.git)
        * [leo_dcerl v0.2.3](https://github.com/leo-project/leo_dcerl.git)
        * [leo_logger v0.10.12](https://github.com/leo-project/leo_logger.git)
        * [leo_mcerl v0.2.4](https://github.com/leo-project/leo_mcerl.git)
        * [leo_mq v0.12.18](https://github.com/leo-project/leo_mq.git)
        * [leo_object_storage v0.14.5](https://github.com/leo-project/leo_object_storage.git)
        * [leo_ordning_reda v0.8.15](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v1.0.2](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.12.14](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.14.4](https://github.com/leo-project/leo_statistics.git)
        * [leo_gateway v0.14.11](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.14.6](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.14.4](https://github.com/leo-project/leo_storage.git)
    * others
        * [bitcask v1.6.2](https://github.com/basho/bitcask.git)
        * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


leofs-0.14.5 (July 8, 2013)
============================

* Bugs Fixed
    * Gateway
        * Gateways may respond an HTTP response with a wrong MIME type as "plain/text" when requested an uppercase filename extension such as "*.JPG" and "*.PNG" from clients
        * Fixed an error handlings properly when processing a large file with disk cache

* Used libraries
    * leo project
        * [leo_backend-db v0.12.17](https://github.com/leo-project/leo_backend_db.git)
        * [leo_cache v0.4.10](https://github.com/leo-project/leo_cache.git)
        * [leo_commons v0.14.4](https://github.com/leo-project/leo_commons.git)
        * [leo_dcerl v0.2.3](https://github.com/leo-project/leo_dcerl.git)
        * [leo_logger v0.10.12](https://github.com/leo-project/leo_logger.git)
        * [leo_mcerl v0.2.4](https://github.com/leo-project/leo_mcerl.git)
        * [leo_mq v0.12.18](https://github.com/leo-project/leo_mq.git)
        * [leo_object_storage v0.14.5](https://github.com/leo-project/leo_object_storage.git)
        * [leo_ordning_reda v0.8.15](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v1.0.2](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.12.14](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.14.4](https://github.com/leo-project/leo_statistics.git)
        * [leo_gateway v0.14.10](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.14.5](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.14.4](https://github.com/leo-project/leo_storage.git)
    * others
        * [bitcask v1.6.2](https://github.com/basho/bitcask.git)
        * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


0.14.4 (June 28, 2013)
=======================

* Improved
    * Supported [Erlang/OTP R16B01](http://www.erlang.org/news/54)
    * Gateway
        * Optimize the slab size for normal use cases

* Used libraries
    * leo project
        * [leo_backend-db v0.12.16](https://github.com/leo-project/leo_backend_db.git)
        * [leo_cache v0.4.9](https://github.com/leo-project/leo_cache.git)
        * [leo_commons v0.14.3](https://github.com/leo-project/leo_commons.git)
        * [leo_dcerl v0.2.3](https://github.com/leo-project/leo_dcerl.git)
        * [leo_logger v0.10.11](https://github.com/leo-project/leo_logger.git)
        * [leo_mcerl v0.2.4](https://github.com/leo-project/leo_mcerl.git)
        * [leo_mq v0.12.17](https://github.com/leo-project/leo_mq.git)
        * [leo_object_storage v0.14.4](https://github.com/leo-project/leo_object_storage.git)
        * [leo_ordning_reda v0.8.14](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v1.0.1](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.12.13](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.14.3](https://github.com/leo-project/leo_statistics.git)
        * [leo_gateway v0.14.7](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.14.3](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.14.3](https://github.com/leo-project/leo_storage.git)
    * others
        * [bitcask v1.6.2](https://github.com/basho/bitcask.git)
        * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


0.14.3 (June 21, 2013)
=======================

* Improved
    * Improved redundant-manager's performance when retrieving redundancies by up to 20%
        * Changed method of retrieving redundancies from *ets* to worker-processes
    * Make a parameter of consumption of queue's message interval into the application config
    * Replace obsolete functions *crypto:sha|md5* with *crypto:hash*

* Fixed Bugs
    * Manager
        * Some problem with bucket names format
    * Storage
        * The number of queue messages can be wrong value
            * Needed to call *bitcask:merge/1* after relaunch the storage process
        * Compaction-function can fail - "did not match data" when a target file is corrupted

* Used libraries
    * leo project
        * [leo_backend-db v0.12.16](https://github.com/leo-project/leo_backend_db.git)
        * [leo_cache v0.4.8](https://github.com/leo-project/leo_cache.git)
        * [leo_commons v0.14.3](https://github.com/leo-project/leo_commons.git)
        * [leo_dcerl v0.2.3](https://github.com/leo-project/leo_dcerl.git)
        * [leo_logger v0.10.11](https://github.com/leo-project/leo_logger.git)
        * [leo_mcerl v0.2.3](https://github.com/leo-project/leo_mcerl.git)
        * [leo_mq v0.12.17](https://github.com/leo-project/leo_mq.git)
        * [leo_object_storage v0.14.4](https://github.com/leo-project/leo_object_storage.git)
        * [leo_ordning_reda v0.8.14](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v1.0.1](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.12.13](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.14.3](https://github.com/leo-project/leo_statistics.git)
        * [leo_gateway v0.14.5](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.14.3](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.14.3](https://github.com/leo-project/leo_storage.git)
    * others
        * [bitcask v1.6.2](https://github.com/basho/bitcask.git)
        * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


0.14.2 (June 7, 2013)
=======================

* New Features
    * Supported rack-awareness replica placement

* Improved
    * Gateway
        * Improved cache-libs, which are mem-cache and disc-cache.
            * Refactoring codes - Getting rid of duplicate codes to "libcutil"
            * Using file:sendfile/2 when retrieving an object from disk cache(dcerl)
        * Improve S3-API's compatibility
            * Supported "Range request API" for a large object
            * Supported retrieving object-list
        * Upgrade Cowboy to v0.8.5
    * Storage
        * Improved data-compaction
            * Performance tuning
            * Avoid polluting OS page caches during compaction
        * Modified the launch process
            * Able to retry connection with manager(s)
    * Manager
        * Supported to Able to remove gateway-node from manager's console when the state of specified node is ‘stop’
        * Supported system-data backup/restore (mnesia)
        * Supported taking over manager(s) for affected hardware failure
        * Supported taking over storage-node when having attach/detach nodes in operation
            * Able to take over from detach-node's files to attach-node

* Fixed Bugs
    * Gateway
        * Reply empty response unintentionally when using some s3-clients

* Used libraries
    * leo project
        * [leo_backend-db v0.12.14](https://github.com/leo-project/leo_backend_db.git)
        * [leo_cache v0.4.7](https://github.com/leo-project/leo_cache.git)
        * [leo_commons v0.14.1](https://github.com/leo-project/leo_commons.git)
        * [leo_dcerl v0.2.2](https://github.com/leo-project/leo_dcerl.git)
        * [leo_logger v0.10.10](https://github.com/leo-project/leo_logger.git)
        * [leo_mcerl v0.2.2](https://github.com/leo-project/leo_mcerl.git)
        * [leo_mq v0.12.15](https://github.com/leo-project/leo_mq.git)
        * [leo_object_storage v0.14.3](https://github.com/leo-project/leo_object_storage.git)
        * [leo_ordning_reda v0.8.13](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.14.2](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.12.10](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.14.2](https://github.com/leo-project/leo_statistics.git)
        * [leo_gateway v0.14.4](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.14.2](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.14.2](https://github.com/leo-project/leo_storage.git)
    * others
        * [bitcask v1.6.2](https://github.com/basho/bitcask.git)
        * [cowboy v0.8.5](https://github.com/extend/cowboy.git)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


0.14.1-1 (May 8, 2013)
=======================

* Improved
    * Gateway
        * Upgrade Ranch and Cowboy to latest version
* Fix Bugs
    * Gateway
        * Reply empty response unintentionally when using some s3 clients
            * "leo_gateway" stored a "0-byte length object" into the cache when put an large-object(over 5MB) from clients. So We supported to remove a registered object from the cache.


0.14.1 (Apr 15, 2013)
=====================

* Improved
    * Gateway
        * Commonize request-handler for easily creating APIs
        * Improve cache-controller which was replaced from ecache to leo_cache
        * Improve performance
            * Upgrade Ranch and Cowboy to latest version
    * Storage
        * Improved phased data-compaction
            * To realize non-blocking data-compaction
                * before: Making compaction per an "object-storage-file"
                * after: Making compaction per an object
    * Manager/Storage
        * Implement judgment of precondition in rebalance-comamnd
        * Implemented ``recover-command`` in Manager
            * To realize:
                * synchronize a file: ``recover file ${file-path}``
                * recover a target-node files: ``recover node ${storage-node}``
                * recover target-node RING: ``recover ring ${storage-node}``
* Fix Bugs
    * NOT worked ``s3cmd`` by degraded
    * Stored wrong file-path with REST-API by degraded

* Used libraries
    * leo project
        * [leo_cache v0.4.4](https://github.com/leo-project/leo_cache.git)
        * [leo_commons v0.12.12](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.12.11](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.14.1](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.12.12](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.11](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.12.19](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.12.8](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.11](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.10.8](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.14.1](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.14.1](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.14.1](https://github.com/leo-project/leo_storage.git)
        * [cherly v0.12.5](https://github.com/leo-project/cherly.git)
        * [dcerl v0.4.1](https://github.com/leo-project/dcerl.git)
    * others
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.8.3](https://github.com/extend/cowboy.git)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


0.14.0 (Mar 20, 2013)
=====================

* Improved
    * Upgrade Cowboy from 0.6.2(foked/add patches) to 0.8.2(original)
        * Improved put/get large-object performance
    * Support recovery of manager-status-monitor after network partition occured

* Fix bugs
    * Recovery objects (Storage)
        * Ignore ``detach-status`` from replication-message
    * Not handle ``compaction-start command`` invalid argument on Manager-console

* Used libraries
    * leo project
        * [leo_commons v0.12.11](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.12.10](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.29](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.12.11](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.10](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.12.18](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.12.7](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.10](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.10.7](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.14.0](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.14.0](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.14.0](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.15](https://github.com/leo-project/ecache.git)
        * [cherly v0.12.4](https://github.com/leo-project/cherly.git)
    * others
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.8.2](https://github.com/extend/cowboy.git)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


0.14.0-RC2 (Mar 14, 2013)
========================

* Improved
    * Improved cache-mechanism (Gateway)
        * Support Layered Cache, First cache-layer is RAM and Secondary cache-layer is SSD (or HDD)
        * This version realized that cache-destination is decided by object-size
             * Need to set [gateway's configuration](http://www.leofs.org/docs/install.html#leofs-gateway)

* Fix bugs
    * Incorrect judgement of active storage-node
        * Possibility of including node(s) of ``attached (status)``

* Used libraries
    * leo project
        * [leo_commons v0.12.11](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.12.10](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.29](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.12.11](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.10](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.12.17](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.12.7](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.10](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.10.7](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.14.0-RC2](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.14.0-RC2](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.14.0-RC2](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.14](https://github.com/leo-project/ecache.git)
        * [cherly v0.12.4](https://github.com/leo-project/cherly.git)
    * others
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)

0.14.0-RC1 (Mar 5, 2013)
========================

* Improved
    * Improved cache-mechanism (Gateway)
        * According to improving efficiency of object-cache-pool, LeoFS-Gateway was able to store an object up to 8MB into the RAM
    * Improved data-compaction related processes (Storage, Manager)
        * Realized compact a part of data-storage, which called "phased data-compaction"
        * Realized ``suspend`` and ``resume`` data-compaction, also able to comfirm status of processes
    * Supported recovery from temporally network unlink #1 (Storage, Manager)
        * Automatically recovered target-node(s), which status transition from ``stop`` to ``running``

* Fix bugs
    * Cannot consume queueing-message(s) when storage cluster has a detached node
        * Resolved that messages of a detached node ignored

* Used libraries
    * leo project
        * [leo_commons v0.12.10](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.12.9](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.28](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.12.9](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.9](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.12.15](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.12.6](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.9](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.10.6](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.14.0-RC1](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.14.0-RC1](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.14.0-RC1](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.8](https://github.com/leo-project/ecache.git)
        * [cherly v0.12.4](https://github.com/leo-project/cherly.git)
    * others
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


0.12.7 (Dec 26, 2012)
---------------------

* Improve performances
    * Storage Performance Tuning#6
        * Related libs: [leo_storage, leo_object_storage]
        * Reduced compaction-cost
             * Able to execution of parallel comaction
                 * Get maximum performance by setting a appropriate number corresponding with number of cores.
* Improve
    * Gateway:
        * The optimal timeout according to file size set up.
        * Modified default cache mode from 'http' to 'inner'
    * Storage:
        * Modified completion of storage for restriction of file destruction
            * When shutting down storage, in charge of storage-process close files
        * Refactor message-queue functions
            * Unified read-failure's queue and write-failure's queue
* Fix bugs
    * Storage:
        * Overwrite an object during rebalance
            * Always check the version(clock) of object
        * Fix haystacks reopen correctly when failing compaction
    * Manager-Console
        * Crush get-endpoints when no-records
        * Crush get-users when no-records

* Used libraries
    * leo project
        * [leo_commons v0.12.6](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.12.2](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.16](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.12.2](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.5](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.12.4](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.12.2](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.6](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.10.3](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.12.9](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.12.9](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.12.9](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.5](https://github.com/leo-project/ecache.git)
        * [cherly v0.12.0](https://github.com/leo-project/cherly.git)
    * others
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlang-lz4)


0.12.5 (Dec 7, 2012)
---------------------

* Improve performances
    * Storage Performance Tuning#5
        * Related libs: [leo_storage, leo_object_storage]
        * Reduced using spawn/1 in replicator #2
* Improve
    * Able to monitor VM-values on Leo Manager's console
    * Deprecate parameterized module in leo_object_storage
        * Reference: http://www.erlang.org/news/35
    * Modified re-launch storage process
        * When regularly stop the storage-process, It writes current status in a file. Then it restarts the storage-process when reading the file.
    * Able to post a large part of an object to LeoFS with multipart-upload API.
* Fix bugs
    * Respond invalid "Etag" from the gateway when using multipart-upload API.
    * Possibility of file-destruction
        * Termination of storages for the restriction of file-destruction
            * Related libs: [leo_storage, leo_object_storage]

* Used libraries
    * leo project
        * [leo_commons v0.12.6](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.12.0](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.12](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.12.0](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.4](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.12.1](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.12.0](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.6](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.10.2](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.12.6](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.12.6](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.12.6](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.3](https://github.com/leo-project/ecache.git)
        * [cherly v0.10.2](https://github.com/leo-project/cherly.git)
    * others
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlang-lz4)


0.12.4 (Nov 21, 2012)
---------------------

* Improve performances
    * Storage Performance Tuning#4
        * Target libs: [leo_storage, leo_object_storage]
        * Reduced using spawn/1 in replicator #1
        * Used rpc:cast/4 for async-replication
* Improve
    * Able to retrieve leofs-uesrs from manager-console
    * NOT allow duplication registration of a user-account into the s3-credential
* Fix bugs
    * Fail rebalance and compaction when exists chunked objects in the storage
    * Manager-console crashed when inputed invalid parameter(s)

* Used libraries
    * leo project
        * [leo_commons v0.12.5](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.10.8](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.11](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.10.5](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.3](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.12.0](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.10.7](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.5](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.10.1](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.12.5](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.12.5](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.12.5](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.2](https://github.com/leo-project/ecache.git)
        * [cherly v0.10.1](https://github.com/leo-project/cherly.git)
    * others
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlang-lz4)


0.12.3 (Nov 9, 2012)
--------------------

* Improve performances
    * Storage Performance Tuning#3
        * Reduced using list_to_binary/1 - [leo_storage, leo_object_storage]
        * Unified internal storage data-type to "binary"
* Improve S3 compatibility
    * Support Multipart upload an object (large-object)
* New feature
    * Able to monitor (SNMP)
        * Sum of objects/storage-node
        * Sum of object-length/storage-node
* Fix bugs
    * Deletion of chunked objects (large-object)
    * Fail rebalance when exists a restarting node
        * Adjust the start timing of RPC for reject requests from remote-node(s)

* Used libraries
    * leo project
        * [leo_commons v0.12.5](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.10.8](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.10](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.10.5](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.3](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.12.0](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.10.6](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.5](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.10.1](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.12.4](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.12.4](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.12.4](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.1](https://github.com/leo-project/ecache.git)
        * [cherly v0.10.0](https://github.com/leo-project/cherly.git)
    * others
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlang-lz4)


0.12.2 (Nov 2, 2012)
--------------------

* Improve performances
    * Storage Performance Tuning#2
        * Revised put operation
* Improve S3 compatibility
    * Support deletion of a bucket
    * Support [s3cmd](http://s3tools.org/s3cmd) (s3-client)
* Fix bugs
    * Gateway:
        * NOT able to store object-cache because data-type is replaced from string to binary with 0.12.1
    * Storage "Compaction"
        * When excuting compact-command, Objects to be removed remain (chunked objects)
* Used libraries
    * leo project
        * [leo_commons v0.12.3](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.10.7](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.9](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.10.4](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.2](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.10.4](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.10.5](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.4](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.10.0](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.12.3](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.12.3](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.12.3](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.1](https://github.com/leo-project/ecache.git)
        * [cherly v0.10.0](https://github.com/leo-project/cherly.git)
    * others
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlang-lz4)


0.12.1  (Oct 25, 2012)
----------------------

* Improve performances
    * Storage Performance Tuning#1
        * Reduced overhead
            * Modified replicator/repairer from gen_server to module
            * Revised data-flow from storage-engine to object/metadata storage
    * Reduced using list_to_binary/1
        * Modified bucket-related libs: [leo_gateway,leo_manager,leo_s3_libs]
    * Compressor/Decompressor replace from snappy to lz4
* Fix bugs
    * Has omissions an object of rebalance
    * Fixed S3 releated:
        * NOT able to remove an endpoint
        * Able to add a bucket with NOT exists access-key

* Used libraries
    * leo project
        * [leo_commons v0.12.0](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.10.4](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.4](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.10.2](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.0](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.10.2](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.10.3](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.2](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.9.8](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.12.1](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.12.2](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.12.1](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.1](https://github.com/leo-project/ecache.git)
        * [cherly v0.10.0](https://github.com/leo-project/cherly.git)
    * others
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlang-lz4)


0.12.0 (Oct 20, 2012)
----------------------

* New feature - Large Object Support
    * Handled from a few bytes an object to a few GB an object
* Improve performances
    * Gateway Performance Tuning
        * HTTP-Server replace from Mochiweb to Cowboy
        * Reduced using list_to_binary/1
    * Revised order of system launch
        * before: Managers -> Storage -> Gateway
        * after : Managers -> Storage|Gateway
    * Changed type of key from string to binary
* Fix bugs
    * S3-API related
        * Overwrited bucket-info by NOT owners
        * When put-operation, NOT returned 'ETag' header
    * Compaction
        * When excuting compact-command, Objects to be removed partly may remain
* Used libraries
    * leo project
        * [leo_commons v0.12.0](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.10.4](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.3](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.10.2](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.6.1](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.10.2](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.10.2](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.1](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.9.7](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.12.0](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.12.0](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.12.0](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.1](https://github.com/leo-project/ecache.git)
        * [cherly v0.10.0](https://github.com/leo-project/cherly.git)
    * others
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlang-lz4)
        * [snappy](https://github.com/fdmanana/snappy-erlang-nif.git)


0.10.2 (Sep 25, 2012)
----------------------

* Improve performances
    * NOT used "proplists:get_value" function
        * Replace from "proplists:get_value/2,3" to "lists:keyfind/2"
    * Related libs:
        * leo_gateway
        * leo_storage
        * leo_manager
        * leo_logger
        * leo_mq
        * leo_object_storage
        * leo_ordning_reda
        * leo_redundant_manager
        * leo_s3_libs
        * leo_statistics
* Improve leo_manager
    * Support output of json-format
    * Support multi-ports TCP-server
        * for CUI console
        * for Application (JSON-Format)
* Improve leo_storage
    * Support plural devices
    * Reduced rebalance/recover costs
        * Support compression of stacked objects and decompresson of received objects (Using snappy)
* Fix bugs
    * leo-gateway related (S3-related):
        * Create bucket from 'Dragon Disk'
        * When using cowboy can send bad values(not iodata)


0.10.1 (Sep 12, 2012)
---------------------

* Improve leo_hex performances
    * "binary_to_hex" function
    * Related libs:
        * leo_gateway
        * leo_object_storage
        * leo_redundant_manager
    * By this correspondence, LeoFS's performance improved 20% up.
* Improve leo_manager
    * Format of output from manager-console
        * Commands:
            * "status"
            * "whereis"
* Improve performance of "storage-stats" in manager-console
* Fix bugs
    * A handling error in S3-libs


0.10.0 (Aug 30, 2012)
---------------------

* Improve S3-API's compatibility
    * Add S3-authentication
    * Add S3-bucket
* Add S3-related command in LeoFS's manager
    * "s3-gen-key" : Generate a S3 key pair(AccessKeyID and SecretAccessKey)
    * "s3-set-endpoint" : Register a new S3 Endpoint
    * "s3-delete-endpoint" : Delete a S3 Endpoint
    * "s3-get-endpoints" : Retrieve all of S3 Endpoints registered
    * "s3-get-buckets" : Retrieve all of Buckets registered
* Improve order of system launch
    * Remove "attach command" in manager - After Storage launched, the node's state is automatically changed to "attached"
* Improve rebalance-function's performance which is about 5 times compare with v0.9.1
* Improve compact-function. Restrain storage's load when compact objects.
* Fix bugs
    * Deletion of Zero bytes in Storage
    * Behavior after the restart of Manager
    * Re-register procs into the Manager's monitor


0.9.1 (Jul 13, 2012)
--------------------

* Remove "apps" directory in leofs - Modified "reltool.config"
* Fix 'shadow vars'
* Remove ZMQ log-appender and AMQP log-appender for leo_logger, They will be provided LeoFS's sub projects.
* Improve - In order to be able to extend "LeoFS's Object Container's file format".


0.9.0  (Jul 4, 2012)
--------------------

* Initial release
