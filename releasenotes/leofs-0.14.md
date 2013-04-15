leofs-0.14.1
============

Features and Improvements for LeoFS
-----------------------------------

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
                * after: Making compaction per some an object
    * Manager/Storage
        * Implement judgment of precondition in rebalance-comamnd
        * Implemented ``recover-command`` in Manager
            * To realize:
                * synchronize a file: ``recover file ${file-path}``
                * recover a target-node files: ``recover node ${storage-node}``
                * recover target-node RING: ``recover ring ${storage-node}``

Bugs Fixed
-----------

* NOT worked ``s3cmd`` by degraded
* Stored wrong file-path with REST-API by degraded

Used Libraries
---------------

* leo project
    * [leo_cache v0.4.0](https://github.com/leo-project/leo_cache.git)
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
* others
    * [bear](htts://github.com/boundary/bear.git)
    * [bitcask](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.3](https://github.com/extend/cowboy.git)
    * [folsom](https://github.com/boundary/folsom.git)
    * [jiffy](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


leofs-0.14.0
============

Features and Improvements for LeoFS
-----------------------------------

* Improved
    * Upgrade Cowboy from 0.6.2(foked/add patches) to 0.8.2(original)
        * Improved put/get large-object performance
    * Support recovery of manager-status-monitor after network partition occured

Bugs Fixed
-----------

* Recovery objects (Storage)
    * Ignore ``detach-status`` from replication-message
* Not handle ``compaction-start command`` invalid argument on Manager-console

Used Libraries
---------------

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
    * [bear](htts://github.com/boundary/bear.git)
    * [bitcask](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.2](https://github.com/extend/cowboy.git)
    * [folsom](https://github.com/boundary/folsom.git)
    * [jiffy](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)



leofs-0.14.0-RC2
================

Features and Improvements for LeoFS
-----------------------------------

* Improved
    * Improved cache-mechanism (Gateway)
        * Support Layered Cache, First cache-layer is RAM and Secondary cache-layer is SSD (or HDD)
        * This version realized that cache-destination is decided by object-size
             * Need to set [gateway's configuration](http://www.leofs.org/docs/install.html#leofs-gateway)

Bugs Fixed
-----------

* Incorrect judgement of active storage-node
    * Possibility of including node(s) of ``attached (status)``

Used Libraries
---------------

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
    * [bear](htts://github.com/boundary/bear.git)
    * [bitcask](https://github.com/basho/bitcask.git)
    * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
    * [folsom](https://github.com/boundary/folsom.git)
    * [jiffy](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


leofs-0.14.0-RC1
================

Features and Improvements for LeoFS
-----------------------------------

* Improved
    * Improved cache-mechanism (Gateway)
        * According to improving efficiency of object-cache-pool, LeoFS-Gateway was able to store an object up to 8MB into the RAM because Gateway can cache chunked objects
    * Improved data-compaction related processes (Storage, Manager)
        * Realized compact a part of data-storage, which called "phased data-compaction"
        * Realized ``suspend`` and ``resume`` data-compaction, also able to comfirm status of processes
    * Supported recovery from temporally network unlink #1 (Storage, Manager)
        * Automatically recover target-node(s), which status transition from ``stop`` to ``running``

Bugs Fixed
-----------

* Cannot consume queueing-message(s) when storage cluster has a detached node
    * Resolved that messages of a detached node ignored

Used Libraries
---------------

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
    * [bear](htts://github.com/boundary/bear.git)
    * [bitcask](https://github.com/basho/bitcask.git)
    * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
    * [folsom](https://github.com/boundary/folsom.git)
    * [jiffy](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)
