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