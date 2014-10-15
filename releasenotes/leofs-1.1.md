1.1.5
=====

Features and Improvements for LeoFS
-----------------------------------

* Improved
    * [#253](https://github.com/leo-project/leofs/issues/253) ``leo_manager`` Inform administrators via a log if the number of replica is changed
    * ``leo_logger`` Implemented that it remove a zero-byte log when closing/rotating

Bugs Fixed
-----------
* [#254](https://github.com/leo-project/leofs/issues/254) ``leo_redundant_manager`` Failed creating RING on cheap VM environment


Used Libraries
---------------

* leo project
    * [leo_backend-db v1.1.2](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.6.0](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.1.0](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.11](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.1.3](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.4.1](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.0.10](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.1.5](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.9](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager 1.9.0](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.10](https://github.com/leo-project/leo_rpc.git)
    * [leo_pod v0.6.2](https://github.com/leo-project/leo_pod.git)
    * [leo_s3_libs v1.1.3](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.7](https://github.com/leo-project/leo_statistics.git)
    * [savanna_agent v0.4.4](https://github.com/leo-project/savanna_agent.git)
    * [savanna_commons v0.8.6](https://github.com/leo-project/savanna_commons.git)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen.git)
    * [nfs_rpc_server v0.2.2](https://github.com/leo-project/nfs_rpc_server.git)
    * [leo_gateway v1.1.5](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.1.5](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.1.5](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.7.0](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [cowlib v0.6.2](https://github.com/extend/cowboy.git)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb.git)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


1.1.4
=====

Features and Improvements for LeoFS
-----------------------------------

* Improved
    * Strictly checked routing-table between manager-node and other nodes
    * Implemented the data diagnosis function of a storage-node
    * [#249](https://github.com/leo-project/leofs/issues/249) ``leo_manager`` Improve whereis command

Bugs Fixed
-----------

* [#246](https://github.com/leo-project/leofs/issues/246) ``leo_storage`` Could not recursively creating directory
* [#247](https://github.com/leo-project/leofs/issues/247) ``leo_storage`` Start normally even if error occur
* [#250](https://github.com/leo-project/leofs/issues/250) ``leo_statistics`` leo_statistics_sampler can be down under very high load


Used Libraries
---------------

* leo project
    * [leo_backend-db v1.1.2](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.6.0](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.1.0](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.11](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.1.2](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.4.1](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.0.10](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.1.5](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.9](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager 1.8.9](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.10](https://github.com/leo-project/leo_rpc.git)
    * [leo_pod v0.6.2](https://github.com/leo-project/leo_pod.git)
    * [leo_s3_libs v1.1.3](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.7](https://github.com/leo-project/leo_statistics.git)
    * [savanna_agent v0.4.4](https://github.com/leo-project/savanna_agent.git)
    * [savanna_commons v0.8.6](https://github.com/leo-project/savanna_commons.git)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen.git)
    * [nfs_rpc_server v0.2.2](https://github.com/leo-project/nfs_rpc_server.git)
    * [leo_gateway v1.1.4](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.1.4](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.1.4](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.7.0](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [cowlib v0.6.2](https://github.com/extend/cowboy.git)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb.git)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


1.1.3
=====

Features and Improvements for LeoFS
-----------------------------------

* Improved
    * Implement the "rollback" command, which is able to rollback state of a storage node from 'detach' to 'running'


Bugs Fixed
-----------

* [#236](https://github.com/leo-project/leofs/issues/236) ``leo_redundant_manager`` - storage node empty after 1.1.2 update and compaction
* [#237](https://github.com/leo-project/leofs/issues/237) ``leo_manager`` - Cannot complete the re-balance objects and the recover-node command
* [#238](https://github.com/leo-project/leofs/issues/238) ``leo_manager`` - Failed to migrate mnesia data when upgrading LeoFS from pre2 to 1.1.2
* [#241](https://github.com/leo-project/leofs/issues/241) ``leo_gateway`` - Change the default ram cache capasity from 1GB to 256MB
* [#242](https://github.com/leo-project/leofs/issues/242) ``leo_gateway`` - leo_garteway process could be down when receiving an invalid request
* [#243](https://github.com/leo-project/leofs/issues/243) ``leo_storage`` - leo_gateway could respond 500 instead of 404

Used Libraries
---------------

* leo project
    * [leo_backend-db v1.1.1](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.4.24](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.0.5](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.10](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.0.5](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.4.0](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.0.9](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.1.3](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.8](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager v1.8.8](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.9](https://github.com/leo-project/leo_rpc.git)
    * [leo_pod v0.6.2](https://github.com/leo-project/leo_pod.git)
    * [leo_s3_libs v1.1.2](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.6](https://github.com/leo-project/leo_statistics.git)
    * [savanna_agent v0.4.3](https://github.com/leo-project/savanna_agent.git)
    * [savanna_commons v0.8.5](https://github.com/leo-project/savanna_commons.git)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen.git)
    * [nfs_rpc_server v0.2.2](https://github.com/leo-project/nfs_rpc_server.git)
    * [leo_gateway v1.1.3](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.1.3](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.1.3](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.7.0](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [cowlib v0.6.2](https://github.com/extend/cowboy.git)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb.git)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


leofs-1.1.2
===========

Features and Improvements for LeoFS
-----------------------------------

* Improved
    * ``leo_object_storage`` - Improved the compaction mechanism
        * It absolutely minimized time required for lock of a storage during data-compaction

Bugs Fixed
-----------

* [#225](https://github.com/leo-project/leofs/issues/225) ``leo_manager`` - The history command may cause VM to crash
        * We've provided the [tools/vacuum_history](https://github.com/leo-project/leofs_utils/tree/develop/tools/vacuum_history)
* [#229](https://github.com/leo-project/leofs/issues/229) ``leo_gateway`` - Can't upload file name with space or other character got 403 forbidden
* [#230](https://github.com/leo-project/leofs/issues/230) ``leo_gateway`` - COPY request fail when the source path include urlencoded characters
* [#234](https://github.com/leo-project/leofs/issues/234) ``leo_gateway`` - Unexpected error may occur when responding 403/404
* [#235](https://github.com/leo-project/leofs/issues/235) ``leo_storage`` - MDC replication could cause VM to crash (in narrow band width network - 100Mbps)

Used Libraries
---------------

* leo project
    * [leo_backend-db v1.1.1](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.4.24](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.0.5](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.10](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.0.5](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.4.0](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.0.9](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.1.3](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.8](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager v1.8.7](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.9](https://github.com/leo-project/leo_rpc.git)
    * [leo_pod v0.6.2](https://github.com/leo-project/leo_pod.git)
    * [leo_s3_libs v1.1.2](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.6](https://github.com/leo-project/leo_statistics.git)
    * [savanna_agent v0.4.3](https://github.com/leo-project/savanna_agent.git)
    * [savanna_commons v0.8.5](https://github.com/leo-project/savanna_commons.git)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen.git)
    * [nfs_rpc_server v0.2.2](https://github.com/leo-project/nfs_rpc_server.git)
    * [leo_gateway v1.1.2](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.1.2](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.1.2](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.7.0](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [cowlib v0.6.2](https://github.com/extend/cowboy.git)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb.git)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/er


leofs-1.1.1
===========

Features and Improvements for LeoFS
-----------------------------------

* Improved
    * [leofs-adm](https://github.com/leo-project/leofs/blob/master/leofs-adm)
        * Supported FreeBSD
        * Able to set *host* and *port* of the Manager node
    * The compaction mechanism
        * Added *raw-file-path* in collapsed-object-log during executing data-compaction

Bugs Fixed
-----------

* [#219](https://github.com/leo-project/leofs/issues/219) ``leo_gateway`` ``leo_s3_libs`` - Access Denied error when putting a file to a bucket with the same name
* [#222](https://github.com/leo-project/leofs/issues/222) ``leo_gateway`` - Discard unnecessary object with the compaction after failure of storing a large object
* [#223](https://github.com/leo-project/leofs/issues/223) ``leo_object_storage`` - Not match checksum of an large-object-parent after executed the rebalance command and the recover-node command
* ``leo_manager`` - Manager node could not modify Gateway node's ring-hash.
    * The issue appeared v1.1.0.
* ``leo_object_storage`` - The compaction command might fail when an object container has broken objects at its head
    * It's absolutely rare case
* ``leo_redundant_manager`` - Manager could not generate RING when taking over objects from a detach-node to a attach-node (a new node)

Used Libraries
---------------

* leo project
    * [leo_backend-db v1.0.8](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.4.23](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.0.4](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.10](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.0.4](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.4.0](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.0.8](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.1.2](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.5](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager v1.8.6](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.8](https://github.com/leo-project/leo_rpc.git)
    * [leo_pod v0.6.2](https://github.com/leo-project/leo_pod.git)
    * [leo_s3_libs v1.1.1](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.5](https://github.com/leo-project/leo_statistics.git)
    * [savanna_agent v0.4.2](https://github.com/leo-project/savanna_agent.git)
    * [savanna_commons v0.8.4](https://github.com/leo-project/savanna_commons.git)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen.git)
    * [nfs_rpc_server v0.2.2](https://github.com/leo-project/nfs_rpc_server.git)
    * [leo_gateway v1.1.1](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.1.1](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.1.1](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.7.0](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [cowlib v0.6.2](https://github.com/extend/cowboy.git)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb.git)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


leofs-1.1.0
===========

Features and Improvements for LeoFS
-----------------------------------

* New Features
    * Implement [NFS Support](http://leo-project.net/leofs/docs/configuration_5.html) as one of LeoFS protocols, which is the alpha version.
        * We have checked this mechanism with *CentOS 6.5* and *Ubuntu Server 14.04 LTS* but we're goinng to investigate other OS such as *FreeBSD* and *SmartOS*.
        * **NOTE:** Changed [the configuration of the protocol](https://github.com/leo-project/leo_gateway/blob/develop/priv/leo_gateway.conf#L46) from ``http.handler`` to ``protocol``
    * The  [leofs-adm](https://github.com/leo-project/leofs/blob/master/leofs-adm) script
        * This command makes administrative operations of LeoFS very easy.
* Improved
    * Refactored all libraries and applications using *dialyzer*

Bugs Fixed
-----------

* [#142](https://github.com/leo-project/leofs/issues/142) ``leo_manager`` - Unknown Behavior - Could not retrieve RING
* [#193](https://github.com/leo-project/leofs/issues/193) ``leo_gateway`` - PUT with REPLACE could leave a source file
* [#199](https://github.com/leo-project/leofs/issues/199) ``leo_storage`` - An object in a sub directory (2 layer and over) could not be removed with DragonDisk
* [#207](https://github.com/leo-project/leofs/issues/207) ``leo_object_storage`` - Compaction may fail when an AVS is corrupted
* [#208](https://github.com/leo-project/leofs/issues/208) ``leo_object_storage`` - Compaction may leave stale objects which size are larger than the chunk size
* ``leo-storage`` Multi data center replication may fail without notifying errors under a narrow bandwidth and very high-load.

Used Libraries
---------------

* LeoProject:
    * [leo_backend-db v1.0.8](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.4.23](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v1.0.4](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.10](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v1.0.4](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.4.0](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v1.0.8](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v1.1.0](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.10.5](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager v1.8.4](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_rpc v0.8.8](https://github.com/leo-project/leo_rpc.git)
    * [leo_pod v0.6.2](https://github.com/leo-project/leo_pod.git)
    * [leo_s3_libs v1.1.0](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v1.0.4](https://github.com/leo-project/leo_statistics.git)
    * [savanna_agent v0.4.2](https://github.com/leo-project/savanna_agent.git)
    * [savanna_commons v0.8.4](https://github.com/leo-project/savanna_commons.git)
    * [erpcgen v0.2.2](https://github.com/leo-project/erpcgen.git)
    * [nfs_rpc_server v0.2.1](https://github.com/leo-project/nfs_rpc_server.git)
    * [leo_gateway v1.1.0](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v1.1.0](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v1.1.0](https://github.com/leo-project/leo_storage.git)

* Others:
    * [bitcask v1.7.0](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [cowlib v0.6.2](https://github.com/extend/cowboy.git)
    * [eleveldb v1.4.10](https://github.com/basho/eleveldb.git)
    * [folsom v0.8.1](https://github.com/boundary/folsom.git)
    * [jiffy v0.8.5](https://github.com/davisp/jiffy.git)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)
