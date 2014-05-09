leofs-1.0.2
===========

Bugs Fixed
-----------

* [#189](https://github.com/leo-project/leofs/issues/189) Actual disk usage is different from the manager-console.
* [#188](https://github.com/leo-project/leofs/issues/188) When existing large objects in a leo_storage, data loss of which happend with the compaction command.


Used Libraries
---------------
* leo project
    * [leo_backend-db v1.0.3](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.4.20](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.0.1](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.7](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.0.2](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.2.9](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.0.3](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.0.4](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.3](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager v1.8.1](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.3](https://github.com/leo-project/leo_rpc.git)
    * [leo_pod v0.6.0](https://github.com/leo-project/leo_pod.git)
    * [leo_s3_libs v1.0.1](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.x](https://github.com/leo-project/leo_statistics.git)
    * [savanna_agent v0.2.2](https://github.com/leo-project/savanna_agent.git)
    * [savanna_commons v0.6.3](https://github.com/leo-project/savanna_commons.git)
    * [leo_gateway v1.0.2](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.0.2](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.0.2](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.6.7](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [eleveldb v1.4.7](https://github.com/basho/eleveldb.git)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


leofs-1.0.1
===========

Features and Improvements for LeoFS
-----------------------------------

* Improved
    * [#183](https://github.com/leo-project/leofs/issues/183) ``leo_storage`` - Replication messages could be sent between storage nodes more than necessary
    * [#176](https://github.com/leo-project/leofs/issues/176) ``leo_center``  - Supported LeoFS v1.0
    * [#167](https://github.com/leo-project/leofs/issues/167) ``leo_storage`` - Automatically synchronization of objects between a local cluster and another remote cluster could fail
    * [#165](https://github.com/leo-project/leofs/issues/165) ``leo_manager`` - Could not modify ``cluster_id``, ``dc_id`` after leo_manger started
    * [#140](https://github.com/leo-project/leofs/issues/140) ``leo_gateway`` - Supported for ``s3gof3r`` as S3-Client

Bugs Fixed
-----------

* Corrected termination of applications
* [#187](https://github.com/leo-project/leofs/issues/187) ``leo_storage`` - Could not work the ``rebalance`` and ``recover`` command
* [#186](https://github.com/leo-project/leofs/issues/186) ``leo_storage`` - Could not correctly finish the ``compaction`` command when exists large-objects
* [#185](https://github.com/leo-project/leofs/issues/185) ``leo_manager`` - Failed to execute the ``delete-endpoint`` command
* [#184](https://github.com/leo-project/leofs/issues/183) ``leo_manager`` - Failed to execute the ``update-acl`` command
* [#182](https://github.com/leo-project/leofs/issues/182) ``leo_manager`` - When status of a storage is ``restarted``, issuing compaction could cause data loss
* [#173](https://github.com/leo-project/leofs/issues/173) ``leo_manager`` - Could not migrate data from leofs-1.0.0-pre3 ro leofs-1.0.0
* [#171](https://github.com/leo-project/leofs/issues/171) ``leo_gateway`` - Head-method with PHP client could not work.
* [#160](https://github.com/leo-project/leofs/issues/160) ``leo_manager`` - Failed to upgrade from v0.14.6 to v1.0.0
* [#153](https://github.com/leo-project/leofs/issues/153) ``leo_gateway`` - When using s3cmd, GET operation for larger than 5MB object gives error at leo_gateway

Used Libraries
---------------

* leo project
    * [leo_backend-db v1.0.3](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.4.20](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.0.1](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.7](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.0.2](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.2.9](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.0.3](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.0.3](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.3](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager v1.8.1](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.3](https://github.com/leo-project/leo_rpc.git)
    * [leo_pod v0.6.0](https://github.com/leo-project/leo_pod.git)
    * [leo_s3_libs v1.0.1](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.x](https://github.com/leo-project/leo_statistics.git)
    * [savanna_agent v0.2.2](https://github.com/leo-project/savanna_agent.git)
    * [savanna_commons v0.6.3](https://github.com/leo-project/savanna_commons.git)
    * [leo_gateway v1.0.1](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.0.1](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.0.1](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.6.7](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [eleveldb v1.4.7](https://github.com/basho/eleveldb.git)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)



leofs-1.0.0
===========

Features and Improvements for LeoFS
-----------------------------------

* New features
    * Multi datacenter replication (1st phase)
        * Realized async-replication between clusters
            * After sent stacked objects to a remote-cluster, receive the list of metadatas from a remote-cluster, then compare them with local-cluster. Eventually, an inconsistent object is recovered.
            * Provided easy operation of multi-datacenter replication with leo-manager's console
            * After joined a new cluster, automatically synchronize objects between local-cluster and a remote-cluster
        * Provided ``recover cluster`` which is able to fix inconsistency objects with remote-cluster
    * [#139](https://github.com/leo-project/leofs/issues/139) Implemented the Bucket ACL

* Improved
    * ``leo_backend_db`` Bump bitcask to v1.6.7
    * ``leo_object_storage`` Updated compaction, handling an object for the multi-dc replication
    * ``leo_ordning_reda`` Updated handle-send interface for the multi-dc replication
    * ``leo_rpc`` Improved to check-in pools into leo_pod


Bugs Fixed
-----------

* [#144](https://github.com/leo-project/leofs/issues/143) Failure occurred when running snmpwalk
* [#146](https://github.com/leo-project/leofs/issues/146) No retry when receiving a head request
* [#148](https://github.com/leo-project/leofs/issues/148) AWS-clients of PHP and Node.js could not copy object properly copied file became 0byte
* [#151](https://github.com/leo-project/leofs/issues/151) ``whereis-command`` had not been recorded on history table
* [#162](https://github.com/leo-project/leofs/issues/162) Able to get removed user on the manager-console
* [#163](https://github.com/leo-project/leofs/issues/163) ``update-acl`` and ``get-acl`` methods unexpected to display the permission messages


Used Libraries
---------------

* leo project
    * [leo_backend-db v1.0.2](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.4.20](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.0.1](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.7](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.0.1](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.2.9](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.0.2](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.0.1](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.2](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager v1.8.0](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.2](https://github.com/leo-project/leo_rpc.git)
    * [leo_pod v0.4.8](https://github.com/leo-project/leo_pod.git)
    * [leo_s3_libs v1.0.1](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.1](https://github.com/leo-project/leo_statistics.git)
    * [savanna_agent v0.2.1](https://github.com/leo-project/savanna_agent.git)
    * [savanna_commons v0.6.2](https://github.com/leo-project/savanna_commons.git)
    * [leo_gateway v1.0.0](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.0.0](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.0.0](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.6.7](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


leofs-1.0.0-pre3
================

Features and Improvements for LeoFS
-----------------------------------

* Improved
    * `leo_manager` Able to plug functions in order to realize to build another manager
    * `leo_redundant_manager` Improved the performance of the membership and the rebalance
    * `leo_redundant_manager` Modified that restrict membership of a node, the state of which is not running
    * `leo_statistics` Switch the function of statistics to [savanna_commons](https://github.com/leo-project/savanna_commons), which isan Erlang based metrics server.
    * `leo_ordning_reda` Improved the function of object stacking
    * `leo_ordning_reda` Improved efficiency of termination of a process

Bugs Fixed
-----------

* `leo_gateway` Fixed to escape xml elements correctly with xmerl_lib:export_text
* `leo_gateway` Modified the http response body format when responding a http error status according to the S3 spec(http://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#RESTErrorResponses)
* `leo_manager` [#116](https://github.com/leo-project/leofs/issues/116) Implement new sync mechanism in Manager, which is able to maintain hard consistency of the rings
* `libcutil (cache-lib)` [#123](https://github.com/leo-project/leofs/issues/123) Add `-fPIC` to CFLAGS
* `leo_backend_db` [#129](https://github.com/leo-project/leofs/issues/129) Fixed to remove all files in a bucket when deleting it(only occured in case using leveldb as metadata storage)
* `leo_gateway` [#130](https://github.com/leo-project/leofs/issues/130) Fixed wrong access log formats when operationg a large-object
* `leo_manager`,`leo_storage`,`leo_gateway` [#131](https://github.com/leo-project/leofs/issues/131) Wrote the version number in the source of configurations
* `leo_gateway` [#136](https://github.com/leo-project/leofs/issues/136) Support that move and copy a large object with S3-Client(s)
* `leo_object_storage` Fixed to handle invalid data blocks while doing compaction
* `leo_rpc` Fixed to close a tcp socket properly in any cases
* `leo_gateway` [#140](https://github.com/leo-project/leofs/issues/140) Fixed to return 206 status when responding a partial body


Used Libraries
---------------

* leo project
    * [leo_backend-db v1.0.0](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.4.19](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.0.0](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.7](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.0.0](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.2.9](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.0.0](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.0.0](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.0](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_pod v0.4.7](https://github.com/leo-project/leo_pod.git)
    * [leo_redundant_manager v1.4.1](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.0](https://github.com/leo-project/leo_rpc.git)
    * [leo_s3_libs v0.12.28](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.0](https://github.com/leo-project/leo_statistics.git)
    * [savanna_commons v0.4.3](https://github.com/leo-project/savanna_commons.git)
    * [leo_gateway v1.0.0-pre3](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.0.0-pre3](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.0.0-pre3](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.6.7](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [eleveldb v1.4.7](https://github.com/basho/eleveldb)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


leofs-1.0.0-pre2
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
