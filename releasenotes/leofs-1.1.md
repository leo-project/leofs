leofs-1.0.2
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

* ``leo_manager`` [#142](https://github.com/leo-project/leofs/issues/142) Unknown Behavior - Could not retrieve RING
* ``leo_gateway`` [#193](https://github.com/leo-project/leofs/issues/193) PUT with REPLACE could leave a source file
* ``leo_storage`` [#199](https://github.com/leo-project/leofs/issues/199) An object in a sub directory (2 layer and over) could not be removed with DragonDisk
* ``leo_object_storage`` [#207](https://github.com/leo-project/leofs/issues/207) Compaction may fail when an AVS is corrupted
* ``leo_object_storage`` [#208](https://github.com/leo-project/leofs/issues/208) Compaction may leave stale objects which size are larger than the chunk size
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
