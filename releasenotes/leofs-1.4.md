1.4.0-pre1
==========

Features and Improvements for LeoFS
-----------------------------------

* [#283](https://github.com/leo-project/leofs/issues/283) ``leo_s3_libs`` Authenticating requests(AWS Signature version4) to be implemented
* [#375](https://github.com/leo-project/leofs/issues/375) ``NFS`` Reduce unnecessary round trips between nfs client and leo_gateway
* [#400](https://github.com/leo-project/leofs/issues/400) ``all`` Use erlang:(max|min) if possible


Bugs Fixed
-----------

* [#370](https://github.com/leo-project/leofs/issues/370) ``s3-api`` ``leo_manager`` ``leo_gateway`` Return wrong http response when handling an invalid bucket format
* [#372](https://github.com/leo-project/leofs/issues/372) ``s3-api`` ``leo_gateway`` Return wrong http response when handling an invalid maxkeys parameter
* [#374](https://github.com/leo-project/leofs/issues/374) ``s3-api`` ``leo_gateway`` Return wrong http response when handling an invalid http headers
* [#381](https://github.com/leo-project/leofs/issues/381) ``leo_gateway`` Does not respond to "List Multipart Uploads" Operation
* [#401](https://github.com/leo-project/leofs/issues/401) ``leo_storage`` 500 error can occur under heavy load with N=1


Used Libraries
---------------

* leo project
    * [leo_backend-db v1.1.10](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.10)
    * [leo_cache v0.6.6](https://github.com/leo-project/leo_cache/releases/tag/0.6.6)
    * [leo_commons v1.1.3](https://github.com/leo-project/leo_commons/releases/tag/1.1.3)
    * [leo_dcerl v0.4.0](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.0)
    * [leo_logger v1.1.6](https://github.com/leo-project/leo_logger/releases/tag/1.1.6)
    * [leo_mcerl v0.6.1](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.1)
    * [leo_mq v1.3.12](https://github.com/leo-project/leo_mq/releases/tag/1.3.12)
    * [leo_object_storage v1.4.0](https://github.com/leo-project/leo_object_storage/releases/tag/1.4.0)
    * [leo_ordning_reda v1.1.1](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.1.1)
    * [leo_redundant_manager 1.9.16](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.16)
    * [leo_rpc v0.10.4](https://github.com/leo-project/leo_rpc/releases/tag/0.10.4)
    * [leo_pod v0.6.6](https://github.com/leo-project/leo_pod/releases/tag/0.6.6)
    * [leo_s3_libs v1.2.0](https://github.com/leo-project/leo_s3_libs/releases/tag/1.2.0)
    * [leo_statistics v1.1.7](https://github.com/leo-project/leo_statistics/releases/tag/1.1.7)
    * [leo_watchdog v0.10.3](https://github.com/leo-project/leo_watchdog/releases/tag/0.10.3)
    * [savanna_agent v0.4.10](https://github.com/leo-project/savanna_agent/releases/tag/0.4.10)
    * [savanna_commons v0.8.13](https://github.com/leo-project/savanna_commons/releases/tag/0.8.13)
    * [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
    * [nfs_rpc_server v0.2.3](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.3)
    * [leo_gateway v1.4.0-pre1](https://github.com/leo-project/leo_gateway/releases/tag/1.4.0-pre1)
    * [leo_manager v1.4.0-pre1](https://github.com/leo-project/leo_manager/releases/tag/1.4.0-pre1)
    * [leo_storage v1.4.0-pre1](https://github.com/leo-project/leo_storage/releases/tag/1.4.0-pre1)
* others
    * [bitcask v2.0.0](https://github.com/lbasho/bitcask/releases/tag/2.0.0)
    * [cowboy v1.0.1](https://github.com/leo-project/cowboy/releases/tag/for-leofs-1.2.11)
    * [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
    * [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
    * [eleveldb v2.1.2](https://github.com/basho/eleveldb/releases/tag/2.1.2)
    * [folsom v0.8.2-for-leofs](https://github.com/leo-project/folsom/releases/tag/0.8.2-for-leofs)
    * [jiffy v0.13.3](https://github.com/davisp/jiffy/releases/tag/0.13.3)
    * [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2)
    * [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)