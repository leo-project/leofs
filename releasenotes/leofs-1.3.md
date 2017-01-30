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
