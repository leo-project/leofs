# 1.4.1 (Apr 24, 2018)
## Fixed Bugs
### LeoGateway

* [#1019](https://github.com/leo-project/leofs/issues/1019) Wrong mime type set for `webm` & `webp`
* [#1021](https://github.com/leo-project/leofs/issues/1021) [S3-API] PUT Object acl should be ignored
* [#1031](https://github.com/leo-project/leofs/issues/1031) [REST-API] CDN integration hasn't worked

### LeoManager

* [#1003](https://github.com/leo-project/leofs/issues/1003) [multi-dc replication] To avoid executing recover-cluster in case of not using multi DC replication


## Improvements

* [#673](https://github.com/leo-project/leofs/issues/673) [gateway] Graceful configuration reload
* [#891](https://github.com/leo-project/leofs/issues/891) [manager] Administrative port on managers shouldn't listen on all interfaces
* [#995](https://github.com/leo-project/leofs/issues/995) [eleveldb] Make log files less fragmented
* [#1018](https://github.com/leo-project/leofs/issues/1018) [libcutil] Fix subunit dependencies for the upcoming Ubuntu-18.04
* [#1035](https://github.com/leo-project/leofs/pull/1035) [leofs-adm] To be able to correctly terminate leofs-adm's commads on Ubuntu-18.04


## Libraries
### Leo Project

* [leo_backend-db v1.2.30](https://github.com/leo-project/leo_backend_db/releases/tag/1.2.30)
* [leo_cache v0.8.10](https://github.com/leo-project/leo_cache/releases/tag/0.8.10)
* [leo_commons v1.2.0](https://github.com/leo-project/leo_commons/releases/tag/1.2.0)
* [leo_dcerl v0.4.9](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.9)
* [leo_logger v1.3.7](https://github.com/leo-project/leo_logger/releases/tag/1.3.7)
* [leo_mcerl v0.6.7](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.7)
* [leo_mq v1.5.17](https://github.com/leo-project/leo_mq/releases/tag/1.5.17)
* [leo_object_storage v1.3.34](https://github.com/leo-project/leo_object_storage/releases/tag/1.3.34)
* [leo_ordning_reda v1.2.10](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.2.10)
* [leo_pod v0.6.9](https://github.com/leo-project/leo_pod/releases/tag/0.6.9)
* [leo_redundant_manager 1.9.60](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.60)
* [leo_rpc v0.10.17](https://github.com/leo-project/leo_rpc/releases/tag/0.10.17)
* [leo_s3_libs v1.2.19](https://github.com/leo-project/leo_s3_libs/releases/tag/1.2.19)
* [leo_statistics v1.1.22](https://github.com/leo-project/leo_statistics/releases/tag/1.1.22)
* [leo_tran v0.2.13](https://github.com/leo-project/leo_tran/releases/tag/0.2.13)
* [leo_watchdog v1.0.6](https://github.com/leo-project/leo_watchdog/releases/tag/1.0.6)
* [savanna_agent v0.4.25](https://github.com/leo-project/savanna_agent/releases/tag/0.4.25)
* [savanna_commons v0.10.11](https://github.com/leo-project/savanna_commons/releases/tag/0.10.11)
* [erpcgen v0.2.6](https://github.com/leo-project/erpcgen/releases/tag/0.2.6)
* [nfs_rpc_server v0.2.6](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.6)

### Others

* [bitcask v2.0.8](https://github.com/leo-project/bitcask/releases/tag/2.0.8.3-for-leofs)
* [cowboy v1.0.0](https://github.com/leo-project/cowboy/tree/for-leofs-1.4)
* [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
* [elarm (ESL)](https://github.com/esl/elarm/commit/5885c906cb7c248f233b90b83f6b910b7b1d293b)
* [eleveldb v2.0.37](https://github.com/basho/eleveldb/releases/tag/2.0.37)
* [folsom v0.8.2-p1](https://github.com/leo-project/folsom/releases/tag/0.8.2-p1)
* [jiffy v0.14.7](https://github.com/davisp/jiffy/releases/tag/0.14.7)
* [recon v2.2.1](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.4.0 (Mar 29, 2018)
### Fixed Bugs
#### LeoGateway

* [#894](https://github.com/leo-project/leofs/issues/894) Immature Body can cause some problem

#### LeoManager

* [#892](https://github.com/leo-project/leofs/issues/892) `delete-bucket` can stop proceeding for a long time
* [#949](https://github.com/leo-project/leofs/issues/949) Suppress error messages in crash.log during mnesia initialization
* [#964](https://github.com/leo-project/leofs/issues/964) `import-user` with the `access-key-id` belonging to a deleted user doesn't work
* [#989](https://github.com/leo-project/leofs/issues/989) Should return `503` in case `leo_watchdog` get triggered
* [#1003](https://github.com/leo-project/leofs/issues/1003) To avoid executing recover-cluster in case of not using multi DC replication
* [#1010](https://github.com/leo-project/leofs/issues/1010) The `whereis` command doesn't work for an object having grandchildren

#### LeoStorage

* [#543](https://github.com/leo-project/leofs/issues/543) `read-repair` don't work when metadata is valid but avs is broken
* [#546](https://github.com/leo-project/leofs/issues/546) `recover-file` don't work when metadata is valid but avs is broken
* [#553](https://github.com/leo-project/leofs/issues/553) GET can be false negative more than necessary
* [#722](https://github.com/leo-project/leofs/issues/722) Errors on gateway/storage when uploading lots of files
* [#758](https://github.com/leo-project/leofs/issues/758) `leo_backend_db` `leo_object_storage` `du/compact-status` can get stuck long time while fetching objects
* [#764](https://github.com/leo-project/leofs/issues/764) `delete-bucket` take much time due to unnecessary congestion
* [#960](https://github.com/leo-project/leofs/issues/960) Suppress error logs on stop
* [#963](https://github.com/leo-project/leofs/issues/963) Suppress error logs on start
* [#966](https://github.com/leo-project/leofs/issues/966) Node entered "strange" state with internal crashes
* [#975](https://github.com/leo-project/leofs/issues/975) `leo_objct_storage` Avoid `gen_server` crash due to timeout
* [#1011](https://github.com/leo-project/leofs/issues/1011) The `delete` API *(S3-API)* doesn't delete grandchildren
* [#1014](https://github.com/leo-project/leofs/issues/1014) `data-compaction` After overwriting a large file and executing data-compaction, cannot retrieve the one
* [#1017](https://github.com/leo-project/leofs/issues/1017) `data-compaction` Grandparent (root) objects can be removed unintentionally

#### Others

* [#971](https://github.com/leo-project/leofs/issues/971) `leo_redundant_manager` Keep dumping the same error in case mq worker process died
* [#977](https://github.com/leo-project/leofs/issues/977) Handle supervisor restart properly *(All Leo's components)*

### Improvements

* [#779](https://github.com/leo-project/leofs/issues/779) To be able to build LeoFS with Erlang/OTP 20
* [#840](https://github.com/leo-project/leofs/issues/840) Support "notify" type of services for `systemd`
* [#941](https://github.com/leo-project/leofs/issues/941) `leo_storage` Making more information available over SNMP for storage nodes
* [#955](https://github.com/leo-project/leofs/issues/955) Supervisor received unexpected message appeared in error log on startup
* [#976](https://github.com/leo-project/leofs/issues/976) Refactor slow processing notification to avoid bottlenecks
* [#980](https://github.com/leo-project/leofs/issues/980) Tune the default 30 min for `TimeoutStartSec`
* [#983](https://github.com/leo-project/leofs/issues/983) `leofs-adm` Add `recover-disk` to improve the recover performance in case of disk drive failures
* [#994](https://github.com/leo-project/leofs/issues/994) `leo_backend_db` Implement `iterater_move` in batch

### Libraries
#### Leo Project

* [leo_backend-db v1.2.29](https://github.com/leo-project/leo_backend_db/releases/tag/1.2.29)
* [leo_cache v0.8.9](https://github.com/leo-project/leo_cache/releases/tag/0.8.9)
* [leo_commons v1.2.0](https://github.com/leo-project/leo_commons/releases/tag/1.2.0)
* [leo_dcerl v0.4.8](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.8)
* [leo_logger v1.3.7](https://github.com/leo-project/leo_logger/releases/tag/1.3.7)
* [leo_mcerl v0.6.6](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.6)
* [leo_mq v1.5.15](https://github.com/leo-project/leo_mq/releases/tag/1.5.15)
* [leo_object_storage v1.3.32](https://github.com/leo-project/leo_object_storage/releases/tag/1.3.32)
* [leo_ordning_reda v1.2.10](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.2.10)
* [leo_pod v0.6.9](https://github.com/leo-project/leo_pod/releases/tag/0.6.9)
* [leo_redundant_manager 1.9.59](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.59)
* [leo_rpc v0.10.17](https://github.com/leo-project/leo_rpc/releases/tag/0.10.17)
* [leo_s3_libs v1.2.19](https://github.com/leo-project/leo_s3_libs/releases/tag/1.2.18)
* [leo_statistics v1.1.22](https://github.com/leo-project/leo_statistics/releases/tag/1.1.22)
* [leo_tran v0.2.13](https://github.com/leo-project/leo_tran/releases/tag/0.2.13)
* [leo_watchdog v1.0.6](https://github.com/leo-project/leo_watchdog/releases/tag/1.0.6)
* [savanna_agent v0.4.25](https://github.com/leo-project/savanna_agent/releases/tag/0.4.25)
* [savanna_commons v0.10.11](https://github.com/leo-project/savanna_commons/releases/tag/0.10.11)
* [erpcgen v0.2.6](https://github.com/leo-project/erpcgen/releases/tag/0.2.6)
* [nfs_rpc_server v0.2.6](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.6)

#### Others

* [bitcask v2.0.8](https://github.com/leo-project/bitcask/releases/tag/2.0.8.3-for-leofs)
* [cowboy v1.0.0](https://github.com/leo-project/cowboy/tree/for-leofs-1.4)
* [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
* [elarm (ESL)](https://github.com/esl/elarm/commit/5885c906cb7c248f233b90b83f6b910b7b1d293b)
* [eleveldb v2.0.36](https://github.com/basho/eleveldb/releases/tag/2.0.36)
* [folsom v0.8.2-p1](https://github.com/leo-project/folsom/releases/tag/0.8.2-p1)
* [jiffy v0.14.7](https://github.com/davisp/jiffy/releases/tag/0.14.7)
* [recon v2.2.1](https://github.com/ferd/recon/releases/tag/2.2.1)
