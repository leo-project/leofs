1.2.4
=====

Bugs Fixed
-----------

* [#297](https://github.com/leo-project/leofs/issues/297) ``leo_object_storage`` Could NOT handle delete requests which made by the MQ mechanism at LeoFS v1.2.3

Used Libraries
---------------

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


1.2.3
=====

Features and Improvements for LeoFS
-----------------------------------

* Improvements
    * [#285](https://github.com/leo-project/leofs/issues/285) ``leo_storage`` Implemented "cluster-watchdog" in order to keep best condition of a cluster
    * [#286](https://github.com/leo-project/leofs/issues/286) ``leo_storage`` Unified name of configuration items for the mq and the compaction

Bugs Fixed
-----------

* [#288](https://github.com/leo-project/leofs/issues/288) ``leo_manager`` Fixed wrong mix/max values when issueing [the status &lt;storage-node&gt; command](http://leo-project.net/leofs/docs/admin_guide/admin_guide_2.html#status-command) with ``leofs-adm command``
* [#290](https://github.com/leo-project/leofs/issues/290) [the du detail &lt;storage-node&gt; command](http://leo-project.net/leofs/docs/admin_guide/admin_guide_5.html#du-detail) with ``leofs-adm command`` had broken 1.2.2
* [#292](https://github.com/leo-project/leofs/issues/292) ``leo_storage`` [proc_lib:start_link](http://erlang.org/doc/man/proc_lib.html#spawn_link-1) must be used instead of [erlang:spawn](http://erlang.org/doc/man/erlang.html#spawn-1)
* [#293](https://github.com/leo-project/leofs/issues/293) Fixed that remain objects after finished data-compaction, situation of which is combination auto-compaction and deletion of a bucket

Used Libraries
---------------

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


1.2.2
=====

Features and Improvements for LeoFS
-----------------------------------

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
      * Improved MQ's confurations for LeoFS Stroage in order to be able to control system load
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

Bugs Fixed
-----------

* [#273](https://github.com/leo-project/leofs/issues/273) ``leo_gateway`` ``NFS`` Max file size was hardcoded as 4GB
* [#274](https://github.com/leo-project/leofs/issues/274) ``leo_manager`` ``leo_storage`` ``leo_gateway`` Crashed a manager node by snmp-related issue with Erlang-17
* [#275](https://github.com/leo-project/leofs/issues/275) ``leo_gateawy`` An error message was output if there is no the http custom header file
* [#277](https://github.com/leo-project/leofs/issues/277) ``leo_gateway`` In REST mode, Putting a large object failed
* [#279](https://github.com/leo-project/leofs/issues/279) ``leo_object_storage`` Compaction status can be inconsistent when an error occured while preparing compaction
* [#282](https://github.com/leo-project/leofs/issues/282) ``leo_gateway`` Gateway's timeout configuration has been ignored

Used Libraries
---------------

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
