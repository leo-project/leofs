# 1.3.8 (Nov 22, 2017)

## Fixed Bugs
### LeoGateway

* [#799](https://github.com/leo-project/leofs/issues/799) s3cmd 2.0 hangs itself and slows down storage nodes
* [#845](https://github.com/leo-project/leofs/issues/845) Errors about multipart object parts on storages during upload
* [#848](https://github.com/leo-project/leofs/issues/848) Gateway returns object that did not change instead of 304 (Not Modified)
* [#851](https://github.com/leo-project/leofs/issues/851) Unable to remove gateway using DNS name through leofs-adm
* [#854](https://github.com/leo-project/leofs/issues/854) Create bucket and put object tests fail
* [#884](https://github.com/leo-project/leofs/issues/884) Omit Content-Length when responding 204
* [#903](https://github.com/leo-project/leofs/issues/903) Correctly uploaded file was removed during multipart upload
* [#907](https://github.com/leo-project/leofs/issues/907) Parallel access to same file locked
* [#926](https://github.com/leo-project/leofs/issues/926) Upload ID can conflict in case multiple clients try to upload a same file at once
* [#937](https://github.com/leo-project/leofs/issues/937) Content-Range Header is not set in range request reply

### LeoManager

* [#914](https://github.com/leo-project/leofs/issues/914) Old Cluster cannot join cluster from 1.3.3, incompatible system_conf sent

### LeoStorage

* [#874](https://github.com/leo-project/leofs/issues/874) Recover-file doesn't work at all
* [#880](https://github.com/leo-project/leofs/issues/880) Recover-node fails to recover all data on storage node
* [#881](https://github.com/leo-project/leofs/issues/881) Read-repairer does not fix all objects

### Others

* [#729](https://github.com/leo-project/leofs/issues/729) `leo_logger` Tons of logs can get LeoFS overloaded
* [#835](https://github.com/leo-project/leofs/issues/835) `leo_gateway` `leo_storage` LeoFS v1.3.7 - MDC not replicating custom headers

## Improvements

* [#841](https://github.com/leo-project/leofs/issues/841) `leo_object_storage` Take much time to open with lots of AVS files
* [#858](https://github.com/leo-project/leofs/issues/858) `leo_gateway` Special URL to check gateway's health in S3 mode for load balancers
* [#883](https://github.com/leo-project/leofs/issues/883) `leo_storage` Increase default open files limit (on Linux)
* [#912](https://github.com/leo-project/leofs/issues/912) `leo_storage` Watchdog for slow processing/timeout always enabled


## Libraries
### Leo Project

* [leo_backend-db v1.2.24](https://github.com/leo-project/leo_backend_db/releases/tag/1.2.24)
* [leo_cache v0.8.7](https://github.com/leo-project/leo_cache/releases/tag/0.8.7)
* [leo_commons v1.1.12](https://github.com/leo-project/leo_commons/releases/tag/1.1.12)
* [leo_dcerl v0.4.8](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.8)
* [leo_logger v1.3.4](https://github.com/leo-project/leo_logger/releases/tag/1.3.4)
* [leo_mcerl v0.6.6](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.6)
* [leo_mq v1.5.12](https://github.com/leo-project/leo_mq/releases/tag/1.5.12)
* [leo_object_storage v1.3.21](https://github.com/leo-project/leo_object_storage/releases/tag/1.3.21)
* [leo_ordning_reda v1.2.8](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.2.8)
* [leo_pod v0.6.9](https://github.com/leo-project/leo_pod/releases/tag/0.6.9)
* [leo_redundant_manager 1.9.55](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.55)
* [leo_rpc v0.10.15](https://github.com/leo-project/leo_rpc/releases/tag/0.10.15)
* [leo_s3_libs v1.2.16](https://github.com/leo-project/leo_s3_libs/releases/tag/1.2.16)
* [leo_statistics v1.1.20](https://github.com/leo-project/leo_statistics/releases/tag/1.1.20)
* [leo_tran v0.2.11](https://github.com/leo-project/leo_tran/releases/tag/0.2.11)
* [leo_watchdog v1.0.4](https://github.com/leo-project/leo_watchdog/releases/tag/1.0.4)
* [savanna_agent v0.4.23](https://github.com/leo-project/savanna_agent/releases/tag/0.4.23)
* [savanna_commons v0.10.9](https://github.com/leo-project/savanna_commons/releases/tag/0.10.9)
* [erpcgen v0.2.6](https://github.com/leo-project/erpcgen/releases/tag/0.2.6)
* [nfs_rpc_server v0.2.6](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.6)

### Others

* [bitcask v2.0.8](https://github.com/leo-project/bitcask/releases/tag/2.0.8.3-for-leofs)
* [cowboy v1.0.0](https://github.com/leo-project/cowboy/tree/for-leofs-1.4)
* [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
* [elarm (ESL)](https://github.com/esl/elarm/commit/5885c906cb7c248f233b90b83f6b910b7b1d293b)
* [eleveldb v2.0.33](https://github.com/basho/eleveldb/releases/tag/2.0.33)
* [folsom v0.8.2-p1](https://github.com/leo-project/folsom/releases/tag/0.8.2-p1)
* [jiffy v0.14.7](https://github.com/davisp/jiffy/releases/tag/0.14.7)
* [recon v2.2.1](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.3.7 (Sep 12, 2017)
## Fixed Bugs

* [#592](https://github.com/leo-project/leofs/issues/592) `leo_storage` `data-compaction` Avoids write operation before leo_storage is not able to execute data-compaction
* [#816](https://github.com/leo-project/leofs/issues/592) `leo_manager` `leo_storage` `mdc-replication` LeoFS 1.3.6 - MDC not replicating data
* [#817](https://github.com/leo-project/leofs/issues/817) `leo_manager` leofs-adm "version all" and "status" hang when node is offline
* [#821](https://github.com/leo-project/leofs/issues/821) `leo_storage` PR [#802](https://github.com/leo-project/leofs/pull/802) break listing objects
* [#827](https://github.com/leo-project/leofs/issues/827) `leofs_package` Nodes fail to start on Ubuntu 16.04 (different sudo setup)


## Libraries
### Leo Project

* [leo_backend-db v1.2.20](https://github.com/leo-project/leo_backend_db/releases/tag/1.2.20)
* [leo_cache v0.8.5](https://github.com/leo-project/leo_cache/releases/tag/0.8.5)
* [leo_commons v1.1.10](https://github.com/leo-project/leo_commons/releases/tag/1.1.10)
* [leo_dcerl v0.4.8](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.8)
* [leo_logger v1.2.6](https://github.com/leo-project/leo_logger/releases/tag/1.2.6)
* [leo_mcerl v0.6.6](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.6)
* [leo_mq v1.5.8](https://github.com/leo-project/leo_mq/releases/tag/1.5.8)
* [leo_object_storage v1.3.17](https://github.com/leo-project/leo_object_storage/releases/tag/1.3.17)
* [leo_ordning_reda v1.2.6](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.2.6)
* [leo_pod v0.6.9](https://github.com/leo-project/leo_pod/releases/tag/0.6.9)
* [leo_redundant_manager 1.9.50](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.50)
* [leo_rpc v0.10.13](https://github.com/leo-project/leo_rpc/releases/tag/0.10.13)
* [leo_s3_libs v1.2.14](https://github.com/leo-project/leo_s3_libs/releases/tag/1.2.14)
* [leo_statistics v1.1.18](https://github.com/leo-project/leo_statistics/releases/tag/1.1.18)
* [leo_watchdog v1.0.2](https://github.com/leo-project/leo_watchdog/releases/tag/1.0.2)
* [savanna_agent v0.4.21](https://github.com/leo-project/savanna_agent/releases/tag/0.4.21)
* [savanna_commons v0.10.7](https://github.com/leo-project/savanna_commons/releases/tag/0.10.7)
* [erpcgen v0.2.6](https://github.com/leo-project/erpcgen/releases/tag/0.2.6)
* [nfs_rpc_server v0.2.6](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.6)

### Others

* [bitcask v2.0.8](https://github.com/leo-project/bitcask/releases/tag/2.0.8.2-for-leofs)
* [cowboy v1.0.0](https://github.com/leo-project/cowboy/tree/for-leofs-1.4)
* [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
* [elarm (ESL)](https://github.com/esl/elarm/commit/5885c906cb7c248f233b90b83f6b910b7b1d293b)
* [eleveldb v2.0.33](https://github.com/basho/eleveldb/releases/tag/2.0.33)
* [folsom v0.8.2-p1](https://github.com/leo-project/folsom/releases/tag/0.8.2-p1)
* [jiffy v0.14.7](https://github.com/davisp/jiffy/releases/tag/0.14.7)
* [recon v2.2.1](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.3.6 (Aug 30, 2017)
## Fixed Bugs

* [#776](https://github.com/leo-project/leofs/issues/776) `leo_watchdog` `leo_storage` Continuous warnings caused by disk usage make `leo_mq stop`
* [#803](https://github.com/leo-project/leofs/issues/803) `leo_storage` Much higher amount of messages during deletion of two buckets at once compared to deleting them one after another
* [#804](https://github.com/leo-project/leofs/issues/804) `leo_storage` `delete-bucket` The state change from enqueuing to monitoring could take much time

## Libraries
### Leo Project

* [leo_backend-db v1.2.20](https://github.com/leo-project/leo_backend_db/releases/tag/1.2.20)
* [leo_cache v0.8.5](https://github.com/leo-project/leo_cache/releases/tag/0.8.5)
* [leo_commons v1.1.10](https://github.com/leo-project/leo_commons/releases/tag/1.1.10)
* [leo_dcerl v0.4.8](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.8)
* [leo_logger v1.2.6](https://github.com/leo-project/leo_logger/releases/tag/1.2.6)
* [leo_mcerl v0.6.6](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.6)
* [leo_mq v1.5.8](https://github.com/leo-project/leo_mq/releases/tag/1.5.8)
* [leo_object_storage v1.3.15](https://github.com/leo-project/leo_object_storage/releases/tag/1.3.15)
* [leo_ordning_reda v1.2.6](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.2.6)
* [leo_pod v0.6.9](https://github.com/leo-project/leo_pod/releases/tag/0.6.9)
* [leo_redundant_manager 1.9.50](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.50)
* [leo_rpc v0.10.13](https://github.com/leo-project/leo_rpc/releases/tag/0.10.13)
* [leo_s3_libs v1.2.14](https://github.com/leo-project/leo_s3_libs/releases/tag/1.2.14)
* [leo_statistics v1.1.18](https://github.com/leo-project/leo_statistics/releases/tag/1.1.18)
* [leo_watchdog v1.0.2](https://github.com/leo-project/leo_watchdog/releases/tag/1.0.2)
* [savanna_agent v0.4.21](https://github.com/leo-project/savanna_agent/releases/tag/0.4.21)
* [savanna_commons v0.10.7](https://github.com/leo-project/savanna_commons/releases/tag/0.10.7)
* [erpcgen v0.2.6](https://github.com/leo-project/erpcgen/releases/tag/0.2.6)
* [nfs_rpc_server v0.2.6](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.6)

### Others

* [bitcask v2.0.8](https://github.com/leo-project/bitcask/releases/tag/2.0.8.2-for-leofs)
* [cowboy v1.0.0](https://github.com/leo-project/cowboy/tree/for-leofs-1.4)
* [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
* [elarm (ESL)](https://github.com/esl/elarm/commit/5885c906cb7c248f233b90b83f6b910b7b1d293b)
* [eleveldb v2.0.33](https://github.com/basho/eleveldb/releases/tag/2.0.33)
* [folsom v0.8.2-p1](https://github.com/leo-project/folsom/releases/tag/0.8.2-p1)
* [jiffy v0.14.7](https://github.com/davisp/jiffy/releases/tag/0.14.7)
* [recon v2.2.1](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.3.5 (Aug 10, 2017)

## Add two leofs-adm commands which relate to LeoFS' bucket operation

- Retrieve a list of the ongoing `delete-bucket`, [delete-bucket-stats](https://leo-project.net/leofs/docs/admin/system_operations/s3/#retrieve-a-list-of-the-ongoing-delete-buckets-delete-bucket-stats)
- Reset a `delete-bucket-stats` record, [reset-delete-bucket-stats](https://leo-project.net/leofs/docs/admin/system_operations/s3/#reset-a-delete-bucket-stats-record-reset-delete-bucket-stats)

## LeoGateway

### NFS
* [#608](https://github.com/leo-project/leofs/issues/608) Set *FSF_HOMOGENEOUS* bit to the properties
* [#609](https://github.com/leo-project/leofs/issues/609) Set ffiles and afiles in the response to fsstat to *MAX*

### Others

* [#730](https://github.com/leo-project/leofs/issues/730) Respond 403 when a multipart upload complete request failed
* [#735](https://github.com/leo-project/leofs/issues/735) Abort multipart upload can respond 204 even if the operation failed
* [#769](https://github.com/leo-project/leofs/issues/769) Abort multipart upload may not delete parts objects
* [#770](https://github.com/leo-project/leofs/issues/770) Remove the code putting a zero byte object when handling abort MPU
* [#784](https://github.com/leo-project/leofs/issues/784) The source object get deleted when `x-amz-metadata-directive` is set to *REPLACE*

## LeoStorage

* [#725](https://github.com/leo-project/leofs/issues/725#issuecomment-302606104) Deleting bucket eventually fails and makes delete queues stuck
    * Some #725's issues remains to be done with v1.4.0
* [#732](https://github.com/leo-project/leofs/issues/732) `leo_mq` `v1.3.3` Items in *QUEUE\_ID\_ASYNC\_DELETION* can keep existing
* [#754](https://github.com/leo-project/leofs/issues/754) *delete-bucket* can not work in case the objects were created on LeoFS <= *v1.3.2.1*
* [#783](https://github.com/leo-project/leofs/issues/783) Consistency Problem with asynchrpnous deletion

## LeoManager

* [#150](https://github.com/leo-project/leofs/issues/150) `s3-api` Implement a robust delete_bucket
* [#556](https://github.com/leo-project/leofs/issues/556) `mnesia` Restarting slave may fail while master is down

## Commons

* [#653](https://github.com/leo-project/leofs/issues/653) Use [timer:send_after/3](http://erlang.org/doc/man/timer.html#send_after-3) for the periodic action in gen_server
* [#731](https://github.com/leo-project/leofs/issues/731) `leo_backend_db` The number `mq-stats` displays can be different from the number leo_backend_db actually stores #731
* [#744](https://github.com/leo-project/leofs/issues/744) `leo_mq` Timeout in return values are not needed at `handle_call`, `handle_cast` in `leo_mq_server`
* [#746](https://github.com/leo-project/leofs/issues/746) `leo_mq` Cache the result of `leo_backend_db_api:status/0` to reduce call times
* [#751](https://github.com/leo-project/leofs/issues/751) `leo_watchdog` LeoWatchdog for disk could make `leo_backend_db` overloaded
* [#761](https://github.com/leo-project/leofs/issues/761) `leo_backend_db` Revision of dependency libraries
* [#774](https://github.com/leo-project/leofs/issues/74) `elarm` records in ets may leak

## Libraries
### Leo Project

* [leo_backend-db v1.2.18](https://github.com/leo-project/leo_backend_db/releases/tag/1.2.18)
* [leo_cache v0.8.3](https://github.com/leo-project/leo_cache/releases/tag/0.8.3)
* [leo_commons v1.1.9](https://github.com/leo-project/leo_commons/releases/tag/1.1.9)
* [leo_dcerl v0.4.6](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.6)
* [leo_logger v1.2.5](https://github.com/leo-project/leo_logger/releases/tag/1.2.5)
* [leo_mcerl v0.6.4](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.4)
* [leo_mq v1.5.6](https://github.com/leo-project/leo_mq/releases/tag/1.5.6)
* [leo_object_storage v1.3.12](https://github.com/leo-project/leo_object_storage/releases/tag/1.3.12)
* [leo_ordning_reda v1.2.4](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.2.4)
* [leo_pod v0.6.8](https://github.com/leo-project/leo_pod/releases/tag/0.6.8)
* [leo_redundant_manager 1.9.48](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.48)
* [leo_rpc v0.10.12](https://github.com/leo-project/leo_rpc/releases/tag/0.10.12)
* [leo_s3_libs v1.2.12](https://github.com/leo-project/leo_s3_libs/releases/tag/1.2.12)
* [leo_statistics v1.1.17](https://github.com/leo-project/leo_statistics/releases/tag/1.1.17)
* [leo_watchdog v1.0.1](https://github.com/leo-project/leo_watchdog/releases/tag/1.0.1)
* [savanna_agent v0.4.20](https://github.com/leo-project/savanna_agent/releases/tag/0.4.20)
* [savanna_commons v0.10.6](https://github.com/leo-project/savanna_commons/releases/tag/0.10.6)
* [erpcgen v0.2.5](https://github.com/leo-project/erpcgen/releases/tag/0.2.5)
* [nfs_rpc_server v0.2.5](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.5)

### Others

* [bitcask v2.0.8](https://github.com/leo-project/bitcask/releases/tag/2.0.8-for-leofs)
* [cowboy v1.0.0](https://github.com/leo-project/cowboy/tree/for-leofs-1.4)
* [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
* [elarm (ESL)](https://github.com/esl/elarm/commit/5885c906cb7c248f233b90b83f6b910b7b1d293b)
* [eleveldb v2.0.33](https://github.com/basho/eleveldb/releases/tag/2.0.33)
* [folsom v0.8.2-p1](https://github.com/leo-project/folsom/releases/tag/0.8.2-p1)
* [jiffy v0.14.7](https://github.com/davisp/jiffy/releases/tag/0.14.7)
* [recon v2.2.1](https://github.com/ferd/recon/releases/tag/2.2.1)

# 1.3.4 (May 3, 2017)
## LeoStorage

- [#644](https://github.com/leo-project/leofs/issues/644) [Bug] Fixed "badarg" from watchdog on storage node in error log
    - Fixes for [#644](https://github.com/leo-project/leofs/issues/644) did not include in v1.3.3 because leo_wachdog's version was wrong.

## Libraries
### Leo Project

* [leo_backend-db v1.2.12](https://github.com/leo-project/leo_backend_db/releases/tag/1.2.12)
* [leo_cache v0.8.3](https://github.com/leo-project/leo_cache/releases/tag/0.8.3)
* [leo_commons v1.1.9](https://github.com/leo-project/leo_commons/releases/tag/1.1.9)
* [leo_dcerl v0.4.6](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.6)
* [leo_logger v1.2.5](https://github.com/leo-project/leo_logger/releases/tag/1.2.5)
* [leo_mcerl v0.6.4](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.4)
* [leo_mq v1.4.17](https://github.com/leo-project/leo_mq/releases/tag/1.4.17)
* [leo_object_storage v1.3.6](https://github.com/leo-project/leo_object_storage/releases/tag/1.3.6)
* [leo_ordning_reda v1.2.4](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.2.4)
* [leo_pod v0.6.8](https://github.com/leo-project/leo_pod/releases/tag/0.6.8)
* [leo_redundant_manager 1.9.39](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.39)
* [leo_rpc v0.10.12](https://github.com/leo-project/leo_rpc/releases/tag/0.10.12)
* [leo_s3_libs v1.2.12](https://github.com/leo-project/leo_s3_libs/releases/tag/1.2.12)
* [leo_statistics v1.1.17](https://github.com/leo-project/leo_statistics/releases/tag/1.1.17)
* [leo_watchdog v0.12.7](https://github.com/leo-project/leo_watchdog/releases/tag/0.12.7)
* [savanna_agent v0.4.20](https://github.com/leo-project/savanna_agent/releases/tag/0.4.20)
* [savanna_commons v0.10.6](https://github.com/leo-project/savanna_commons/releases/tag/0.10.6)
* [erpcgen v0.2.5](https://github.com/leo-project/erpcgen/releases/tag/0.2.5)
* [nfs_rpc_server v0.2.5](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.5)

### Others

* [bitcask v2.0.7](https://github.com/basho/bitcask/releases/tag/2.0.7)
* [cowboy v1.0.0](https://github.com/leo-project/cowboy/tree/for-leofs-1.4)
* [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
* [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
* [eleveldb v2.0.33](https://github.com/basho/eleveldb/releases/tag/2.0.33)
* [folsom v0.8.2-p1](https://github.com/leo-project/folsom/releases/tag/0.8.2-p1)
* [jiffy v0.14.7](https://github.com/davisp/jiffy/releases/tag/0.14.7)
* [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.3.3
## LeoStorage

- [#573](https://github.com/leo-project/leofs/issues/573) [Improve] To be configurable the data synchronization
- [#590](https://github.com/leo-project/leofs/issues/590) [Improve] API to break a data block in AVS
- [#690](https://github.com/leo-project/leofs/issues/690) [Improve] Track Slow I/O on `leo_compact_fsm_worker`
- [#593](https://github.com/leo-project/leofs/issues/593) [Bug] Fixed that reduce not_found logs
- [#624](https://github.com/leo-project/leofs/issues/624) [Bug] Fixed that messages in a queue can get lost
- [#645](https://github.com/leo-project/leofs/issues/645) [Bug][MDC-Replication] `read-repair`, `data-compaction`, `data-recovery` feature may replicate objects more than `mdc_replication.num_of_replicas_a_dc`
- [#641](https://github.com/leo-project/leofs/issues/660) [Bug] Fixed that MDC replicated objects are missing HTTP headers after upgrading from 1.3.0 to 1.3.2.1
- ~~[#644](https://github.com/leo-project/leofs/issues/644) [Bug] Fixed "badarg" from watchdog on storage node in error log~~
- [#668](https://github.com/leo-project/leofs/issues/668) [Bug] Fixed that `leo_storage` can be rebooted when lots of badarg from eleveldb happened
- [#704](https://github.com/leo-project/leofs/issues/704) [Bug] Fixed that possibility of MQ's data loss
- [#713](https://github.com/leo-project/leofs/issues/713) [Bug] Fixed that `replicator/5` can receive messages generated by `gen_server:call/cast`
- [#660](https://github.com/leo-project/leofs/issues/660) [Maintenance] Needs to deliver moving AVS directory after launching

## LeoGateway
### NFS

- [#327](https://github.com/leo-project/leofs/issues/327) [Improve] Return file attributes on every operation
- [#598](https://github.com/leo-project/leofs/issues/598) [Improve] Make the response to fsinfo configurable
- [#613](https://github.com/leo-project/leofs/issues/613) [Improve] `nfs_fh3` is not needed to be true random
- [#614](https://github.com/leo-project/leofs/issues/614) [Improve] Avoid concating list in `leo_nfs_file_handler:list_dir`
- [#615](https://github.com/leo-project/leofs/issues/615) [Improve] `large_obj_delete_chunks` should keep iterating even some error happened
- [#596](https://github.com/leo-project/leofs/issues/596) [Bug] Fixed that the total file size can be wrong when writing the same file in parallel
- [#599](https://github.com/leo-project/leofs/issues/599) [Bug] Fixed that some responses don't comply [rfc1813](https://www.ietf.org/rfc/rfc1813.txt)
- [#604](https://github.com/leo-project/leofs/issues/604) [Bug] Fixed that `NFS_DUMMY_FILE4S3DIR` should be replaced with one including reserved characters
- [#611](https://github.com/leo-project/leofs/issues/611) [Bug] Fixed that miss error handling in `nfsproc3_rmdir_3`
- [#612](https://github.com/leo-project/leofs/issues/612) [Bug] Fixed that `readdir` entries can leak when NFS client crash

### S3-API

- [#483](https://github.com/leo-project/leofs/issues/483) [Bug] Fixed that `S3 Sync` feature does not synchronize directories properly
- [#642](https://github.com/leo-project/leofs/issues/642) [Bug] Fixed that failed to fetch a bucket info while both managers are down after bucket_prop_sync_interval passed
- [#647](https://github.com/leo-project/leofs/issues/647) [Bug] Fixed that make `find_bucket_by_name` fail-safe as [#642](https://github.com/leo-project/leofs/issues/642)

## LeoManager

- [#695](https://github.com/leo-project/leofs/issues/695) [Bug] Fixed that degrading the get-bucket command
- [#606](https://github.com/leo-project/leofs/issues/606) [Improve] Able to create a user with `access-key` and `secret-access-key`
- [#607](https://github.com/leo-project/leofs/issues/607) [Improve] Make a user password more secure

## Commons

- [#626](https://github.com/leo-project/leofs/issues/626) [Bug] Fixed that missing error handlings of `leo_ordning_reda`
- [#643](https://github.com/leo-project/leofs/issues/643) [Bug] Fixed that a way to remotely check system_version on nodes with the `status` command
- [#712](https://github.com/leo-project/leofs/issues/712) [Bug] Fixed that iterator-handler can be leaked
- [#667](https://github.com/leo-project/leofs/issues/667) [Improve] Set `ERL_CRASH_DUMP_SECONDS` to a negative value
- [#671](https://github.com/leo-project/leofs/issues/671) [Improve][leo\_backend\_db] Implement status callback for eleveldb
- [#617](https://github.com/leo-project/leofs/issues/617) [Package] `/usr/local/bin/leofs-adm` should be removed
- [#638](https://github.com/leo-project/leofs/issues/617) [Package] Running packaged version as non-privileged user

## Libraries
### Leo Project

* [leo_backend-db v1.2.12](https://github.com/leo-project/leo_backend_db/releases/tag/1.2.12)
* [leo_cache v0.8.3](https://github.com/leo-project/leo_cache/releases/tag/0.8.3)
* [leo_commons v1.1.9](https://github.com/leo-project/leo_commons/releases/tag/1.1.9)
* [leo_dcerl v0.4.6](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.6)
* [leo_logger v1.2.5](https://github.com/leo-project/leo_logger/releases/tag/1.2.5)
* [leo_mcerl v0.6.4](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.4)
* [leo_mq v1.4.17](https://github.com/leo-project/leo_mq/releases/tag/1.4.17)
* [leo_object_storage v1.3.6](https://github.com/leo-project/leo_object_storage/releases/tag/1.3.6)
* [leo_ordning_reda v1.2.4](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.2.4)
* [leo_pod v0.6.8](https://github.com/leo-project/leo_pod/releases/tag/0.6.8)
* [leo_redundant_manager 1.9.39](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.39)
* [leo_rpc v0.10.12](https://github.com/leo-project/leo_rpc/releases/tag/0.10.12)
* [leo_s3_libs v1.2.12](https://github.com/leo-project/leo_s3_libs/releases/tag/1.2.12)
* [leo_statistics v1.1.17](https://github.com/leo-project/leo_statistics/releases/tag/1.1.17)
* [leo_watchdog v0.12.6](https://github.com/leo-project/leo_watchdog/releases/tag/0.12.6)
* [savanna_agent v0.4.20](https://github.com/leo-project/savanna_agent/releases/tag/0.4.20)
* [savanna_commons v0.10.6](https://github.com/leo-project/savanna_commons/releases/tag/0.10.6)
* [erpcgen v0.2.5](https://github.com/leo-project/erpcgen/releases/tag/0.2.5)
* [nfs_rpc_server v0.2.5](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.5)

### Others

* [bitcask v2.0.7](https://github.com/basho/bitcask/releases/tag/2.0.7)
* [cowboy v1.0.0](https://github.com/leo-project/cowboy/tree/for-leofs-1.4)
* [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
* [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
* [eleveldb v2.0.33](https://github.com/basho/eleveldb/releases/tag/2.0.33)
* [folsom v0.8.2-p1](https://github.com/leo-project/folsom/releases/tag/0.8.2-p1)
* [jiffy v0.14.7](https://github.com/davisp/jiffy/releases/tag/0.14.7)
* [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.3.2

## Improvements

* [#509](https://github.com/leo-project/leofs/issues/509) [boto3](https://github.com/boto/boto3) which is an S3 client was supported
* [#577](https://github.com/leo-project/leofs/issues/577) An Erlang VM's parameter, `eager check I/O scheduling` is enabled

## Fixed Bugs

* LeoGateway:
    * [#489](https://github.com/leo-project/leofs/issues/489) `leo_gateway` Cannot integrate LeoFS with Hadoop
        * `transfer-encoding`, Identity in [Cowboy](https://github.com/leo-project/cowboy/)
    * [#564](https://github.com/leo-project/leofs/issues/564) `leo_gateway` `v1.3.1` Large object put handler could halt infinitely
    * [#566](https://github.com/leo-project/leofs/issues/566) `leo_gateway` After upgrading to v1.3.1, breaks file listing of a bucket, a result of which is empty
    * [#570](https://github.com/leo-project/leofs/issues/570) `leo_gateway` Large Object Handler consumes much memory and causes out of memory(OOM) eventually
* LeoStorage:
    * [#581](https://github.com/leo-project/leofs/issues/581) `leo_storage` Unit test failure with Erlang/OTP 17.x and 18.x
        * Bumped [meck which is a mocking library for Erlang](https://github.com/eproxus/meck)
    * [#586](https://github.com/leo-project/leofs/issues/586) `leo_storage` `leo_object_storage` A LeoStorage's configuration, `is_strict_check` is ignored
    * [#589](https://github.com/leo-project/leofs/issues/589) `leo_storage` `read_repair` Not satisfy the read-consistency
* LeoManager:
    * [#583](https://github.com/leo-project/leofs/issues/583) `leo_manager` Delete Bucket does not remove the objects stored in it, `v1.3.0` and `v1.3.1` is affected

## Used libraries
### Leo Project

* [leo_backend-db v1.2.9](https://github.com/leo-project/leo_backend_db/releases/tag/1.2.9)
* [leo_cache v0.8.3](https://github.com/leo-project/leo_cache/releases/tag/0.8.3)
* [leo_commons v1.1.9](https://github.com/leo-project/leo_commons/releases/tag/1.1.9)
* [leo_dcerl v0.4.6](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.6)
* [leo_logger v1.2.5](https://github.com/leo-project/leo_logger/releases/tag/1.2.5)
* [leo_mcerl v0.6.3](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.3)
* [leo_mq v1.4.12](https://github.com/leo-project/leo_mq/releases/tag/1.4.12)
* [leo_object_storage v1.2.31](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.31)
* [leo_ordning_reda v1.2.4](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.2.4)
* [leo_pod v0.6.8](https://github.com/leo-project/leo_pod/releases/tag/0.6.8)
* [leo_redundant_manager 1.9.29](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.32)
* [leo_rpc v0.10.12](https://github.com/leo-project/leo_rpc/releases/tag/0.10.12)
* [leo_s3_libs v1.2.9](https://github.com/leo-project/leo_s3_libs/releases/tag/1.2.9)
* [leo_statistics v1.1.17](https://github.com/leo-project/leo_statistics/releases/tag/1.1.17)
* [leo_watchdog v0.12.6](https://github.com/leo-project/leo_watchdog/releases/tag/0.12.6)
* [savanna_agent v0.4.20](https://github.com/leo-project/savanna_agent/releases/tag/0.4.20)
* [savanna_commons v0.10.6](https://github.com/leo-project/savanna_commons/releases/tag/0.10.6)
* [erpcgen v0.2.4](https://github.com/leo-project/erpcgen/releases/tag/0.2.4)
* [nfs_rpc_server v0.2.4](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.4)

### Others

* [bitcask v2.0.7](https://github.com/basho/bitcask/releases/tag/2.0.7)
* [cowboy v1.0.0](https://github.com/leo-project/cowboy/tree/for-leofs-1.4)
* [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
* [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
* [eleveldb v2.0.33](https://github.com/basho/eleveldb/releases/tag/2.0.33)
* [folsom v0.8.2-p1](https://github.com/leo-project/folsom/releases/tag/0.8.2-p1)
* [jiffy v0.14.7](https://github.com/davisp/jiffy/releases/tag/0.14.7)
* [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.3.1
## Improvements

* [#107](https://github.com/leo-project/leofs/issues/107) Custom metadata support
* [#486](https://github.com/leo-project/leofs/issues/486) Erlang/OTP 19 support
* [#492](https://github.com/leo-project/leofs/issues/492) `leo_gateway` Improve `access-log` of LeoGateway to recognize state of a request
* [#493](https://github.com/leo-project/leofs/issues/493) `leo_storage` Improve `error-log` and `warning-log` to recognize state of a request
* [#502](https://github.com/leo-project/leofs/issues/502) `leo_storage` Slow operation logging does not account for queue time

## Fixed Bugs

* LeoGateway:
    * [#491](https://github.com/leo-project/leofs/issues/491) `leo_gateway` `rest` Error when request URL does not contain path to file
    * [#506](https://github.com/leo-project/leofs/issues/506) `leo_gateway` PUT a large object without using multipart upload spends much memory
    * [#529](https://github.com/leo-project/leofs/issues/529) `leo_gateway``cache` Hidden memory cache capacity and object size limit
    * [#531](https://github.com/leo-project/leofs/issues/531) `leo_gateway``s3``rest` The last part of a large object can be broken with **reading_chunked_obj_len > chunked_obj_len** in `leo_gateway.conf`
    * NFS:
        * `leo_gateway``nfs` Disk space can be wrong
            * [#508](https://github.com/leo-project/leofs/issues/508)
            * [#533](https://github.com/leo-project/leofs/issues/533)
        * [#536](https://github.com/leo-project/leofs/issues/536) `leo_gateway``nfs` Calculating disk space needs to depend on a number of replicas
        * [#537](https://github.com/leo-project/leofs/issues/537) `leo_gateway``nfs` Uploading a big file may spend much time
        * [#539](https://github.com/leo-project/leofs/issues/531) `leo_gateway``nfs` Overlooking error handlings in case updating part of a large file
* LeoStorage:
    * Data compaction related issues:
        * [#511](https://github.com/leo-project/leofs/issues/511) `leo_object_storage` Failed to rollback the compaction status to `idle` from `running`
        * [#520](https://github.com/leo-project/leofs/issues/520) `leo_object_storage` Consume the cpu more than necessary during skipping a garbage block
        * [#521](https://github.com/leo-project/leofs/issues/521) `leo_object_storage` Elements in `pid_pairs` may leak when the exec time of `data-compaction` and `data-diagnosis` is too short
        * [#522](https://github.com/leo-project/leofs/issues/522) `leo_object_storage` "leo_compact_worker_X" gets stuck during skipping a garbage block
        * [#523](https://github.com/leo-project/leofs/issues/523) `leo_object_storage` `data-compaction` and `data-diagnosis` may get slow down when a garbage exists at the end of an AVS(leo_object_storage's container)
        * [#524](https://github.com/leo-project/leofs/issues/524) `leo_object_storage` `leo_compact_fsm_controller` causes the compaction status inconsistent when executing `data-compaction` and `data-diagnosis` in parallel
        * [#526](https://github.com/leo-project/leofs/issues/526) `leo_object_storage` `leo_fsm_compact_worker:terminate` don't free the resources properly
        * [#527](https://github.com/leo-project/leofs/issues/527) `leo_object_storage` More strictly checking the header, less file:pread(s) for reading a body
    * [#515](https://github.com/leo-project/leofs/issues/515) `leo_storage``read-repair` Performance Issue when primary replica is out-dated
    * [#544](https://github.com/leo-project/leofs/issues/544) `leo_storage``read-repair` Unnecessary check happened
    * [#545](https://github.com/leo-project/leofs/issues/560) `leo_storage``read-repair` Part of errors may be not output on `error-log`
    * [#547](https://github.com/leo-project/leofs/issues/547) `leo_storage``recover-file` With a filename that acutually doesn't exist cause mq get stuck
    * [#555](https://github.com/leo-project/leofs/issues/555) `leo_object_storage``leo_backend_db``leveldb` LeoFS's LevelDB settings are hard-coded and conservative
* LeoManager:
    * [#560](https://github.com/leo-project/leofs/issues/560) `leo_manager` "already_started" always appeared in `error.log` on managers
* Common libs:
    * [#476](https://github.com/leo-project/leofs/issues/476) `libcutil``leo_cache` [Ubuntu 16.04 LTS](http://releases.ubuntu.com/16.04/) support
    * [#494](https://github.com/leo-project/leofs/issues/494) `watchdog``iostat` result parsing error


## Used libraries
### Leo Project

* [leo_backend-db v1.2.6](https://github.com/leo-project/leo_backend_db/releases/tag/1.2.6)
* [leo_cache v0.8.2](https://github.com/leo-project/leo_cache/releases/tag/0.8.2)
* [leo_commons v1.1.8](https://github.com/leo-project/leo_commons/releases/tag/1.1.8)
* [leo_dcerl v0.4.6](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.6)
* [leo_logger v1.2.4](https://github.com/leo-project/leo_logger/releases/tag/1.2.4)
* [leo_mcerl v0.6.3](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.3)
* [leo_mq v1.4.9](https://github.com/leo-project/leo_mq/releases/tag/1.4.9)
* [leo_object_storage v1.2.28](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.28)
* [leo_ordning_reda v1.2.2](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.2.2)
* [leo_pod v0.6.7](https://github.com/leo-project/leo_pod/releases/tag/0.6.7)
* [leo_redundant_manager 1.9.29](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.29)
* [leo_rpc v0.10.10](https://github.com/leo-project/leo_rpc/releases/tag/0.10.10)
* [leo_s3_libs v1.2.7](https://github.com/leo-project/leo_s3_libs/releases/tag/1.2.7)
* [leo_statistics v1.1.14](https://github.com/leo-project/leo_statistics/releases/tag/1.1.14)
* [leo_watchdog v0.12.5](https://github.com/leo-project/leo_watchdog/releases/tag/0.12.5)
* [savanna_agent v0.4.17](https://github.com/leo-project/savanna_agent/releases/tag/0.4.17)
* [savanna_commons v0.10.3](https://github.com/leo-project/savanna_commons/releases/tag/0.10.3)
* [erpcgen v0.2.4](https://github.com/leo-project/erpcgen/releases/tag/0.2.4)
* [nfs_rpc_server v0.2.4](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.4)
* [leo_gateway v1.3.1](https://github.com/leo-project/leo_gateway/releases/tag/1.3.1)
* [leo_manager v1.3.1](https://github.com/leo-project/leo_manager/releases/tag/1.3.1)
* [leo_storage v1.3.1](https://github.com/leo-project/leo_storage/releases/tag/1.3.1)

## Others

* [bitcask v2.0.6](https://github.com/basho/bitcask/releases/tag/2.0.6)
* [cowboy v1.0.0](https://github.com/leo-project/cowboy/tree/for-leofs-1.4)
* [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
* [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
* [eleveldb v2.0.33](https://github.com/basho/eleveldb/releases/tag/2.0.33)
* [folsom v0.8.2-for-leofs](https://github.com/leo-project/folsom/releases/tag/0.8.2-for-leofs)
* [jiffy v0.14.7](https://github.com/davisp/jiffy/releases/tag/0.14.7)
* [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


# 1.3.0
## Improvements

* AWS Signature v4 support
    * [#283](https://github.com/leo-project/leofs/issues/283) `leo_s3_libs` Authenticating requests(AWS Signature version4) to be implemented
    * [#373](https://github.com/leo-project/leofs/issues/373) Supported `aws-sdk-go`
* [#436](https://github.com/leo-project/leofs/issues/436) `v1.3``nfs` Everyone is able to operate objects under a bucket with NFS
* [#479](https://github.com/leo-project/leofs/issues/479) `leo_gateway` Output access-logs, retrieving object list and removing a directory

## Fixed Bugs

* [#482](https://github.com/leo-project/leofs/issues/482) `leo_gateway` A Delete request failed when `cache.http_cache = true`
* [#485](https://github.com/leo-project/leofs/issues/485) `leo_watchdog` Not able to clear state of a cpu's watchdog due to unexpected [Erlang cpu_sup](http://erlang.org/doc/man/cpu_sup.html)'s error

## Used libraries
### Leo Project

* [leo_backend-db v1.2.2](https://github.com/leo-project/leo_backend_db/releases/tag/1.2.2)
* [leo_cache v0.8.0](https://github.com/leo-project/leo_cache/releases/tag/0.8.0)
* [leo_commons v1.1.6](https://github.com/leo-project/leo_commons/releases/tag/1.1.6)
* [leo_dcerl v0.4.5](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.5)
* [leo_logger v1.2.2](https://github.com/leo-project/leo_logger/releases/tag/1.2.2)
* [leo_mcerl v0.6.3](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.3)
* [leo_mq v1.4.2](https://github.com/leo-project/leo_mq/releases/tag/1.4.5)
* [leo_object_storage v1.2.22](https://github.com/leo-project/leo_object_storage/releases/tag/1.2.22)
* [leo_ordning_reda v1.2.0](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.2.0)
* [leo_redundant_manager 1.9.26](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.26)
* [leo_rpc v0.10.8](https://github.com/leo-project/leo_rpc/releases/tag/0.10.8)
* [leo_pod v0.6.6](https://github.com/leo-project/leo_pod/releases/tag/0.6.6)
* [leo_s3_libs v1.1.13](https://github.com/leo-project/leo_s3_libs/releases/tag/1.1.13)
* [leo_statistics v1.1.12](https://github.com/leo-project/leo_statistics/releases/tag/1.1.12)
* [leo_watchdog v0.12.3](https://github.com/leo-project/leo_watchdog/releases/tag/0.12.3)
* [savanna_agent v0.4.15](https://github.com/leo-project/savanna_agent/releases/tag/0.4.15)
* [savanna_commons v0.10.1](https://github.com/leo-project/savanna_commons/releases/tag/0.10.1)
* [erpcgen v0.2.4](https://github.com/leo-project/erpcgen/releases/tag/0.2.4)
* [nfs_rpc_server v0.2.4](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.4)
* [leo_gateway v1.2.22](https://github.com/leo-project/leo_gateway/releases/tag/1.2.22)
* [leo_manager v1.2.22](https://github.com/leo-project/leo_manager/releases/tag/1.2.22)
* [leo_storage v1.2.22](https://github.com/leo-project/leo_storage/releases/tag/1.2.22)

### Others

* [bitcask v2.0.1](https://github.com/basho/bitcask/releases/tag/2.0.1)
* [cowboy v1.0.0](https://github.com/leo-project/cowboy/releases/tag/1.0.0-p1)
* [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
* [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
* [eleveldb v2.1.10](https://github.com/basho/eleveldb/releases/tag/2.1.10)
* [folsom v0.8.2-for-leofs](https://github.com/leo-project/folsom/releases/tag/0.8.2-for-leofs)
* [jiffy v0.14.7](https://github.com/davisp/jiffy/releases/tag/0.14.7)
* [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)
