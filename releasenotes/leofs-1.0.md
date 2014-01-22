leofs-1.0.0-pre1
================

Features and Improvements for LeoFS
-----------------------------------

* New Features
     * Implemented for Multi DC Replication
         * Synchronize configuration and staus of remote cluster(s) after joined each other
         * Regularly check status of remote cluster(s)
         * Add the commands of Multi DC Replication in Manager
              * 'join-cluster' and 'remove-cluster'
* Improved
     * The performance of the prefix search, which like a "ls"
     * Changed format of the metadata because of improvement of the prefix search
     * Removed Elasticsearch integration in the logger

Bugs Fixed
-----------

* Unexpected response from leo_storage to leo_gateway when an object wan not found
* Respond an error with deletion of an object when it was not found


Used Libraries
---------------

* leo project
    * [leo_backend-db v0.14.5](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.4.17](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v0.14.10](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.7](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v0.12.8](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.2.9](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v0.12.25](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v0.14.11](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.8.19](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_pod v0.4.7](https://github.com/leo-project/leo_pod.git)
    * [leo_redundant_manager v1.4.0](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.6.2](https://github.com/leo-project/leo_rpc.git)
    * [leo_s3_libs v0.12.27](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v0.14.9](https://github.com/leo-project/leo_statistics.git)
    * [leo_gateway v1.0.0-pre2](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.0.0-pre2](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.0.0-pre2](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.6.4](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [folsom v0.8.0](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


leofs-1.0.0-pre1
================

Features and Improvements for LeoFS
-----------------------------------

* Improved
   * Revised the replicator and the read-repairer for the multi data center replicaion
   * [#113](https://github.com/leo-project/leofs/issues/113) Modified the configuration of limit of large-object length
      * "limit length" = ${large_object.chunked_obj_len} * ${large_object.max_chunked_objs}
   * Improve the function of "rebalance-command"
   * Improve the function of "start-command"
   * Added "dump-ring" on the manager console

Bugs Fixed
-----------

* Fixed that a rebalance msg can be lost during rebalance with a 0.02% possibility
* Fixed that not retrieved a bucket-info when stopping all managers

Used Libraries
---------------

* leo project
    * [leo_backend-db v0.14.2](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.4.16](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v0.14.9](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.7](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v0.12.6](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.2.9](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v0.12.23](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v0.14.8](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.8.18](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager v1.2.6](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_s3_libs v0.12.25](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v0.14.8](https://github.com/leo-project/leo_statistics.git)
    * [leo_gateway v1.0.0-pre1](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.0.0-pre1](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.0.0-pre1](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.6.4](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [folsom v0.8.0](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)
