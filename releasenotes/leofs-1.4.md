## 1.4.0-pre.3
### Features and Improvements for LeoFS

* The erasure coding support
	* Released [Leo's erasure code lib - leo_erasure](https://github.com/leo-project/leo_erasure)
	* The libraries of the erasure code
		* Supported [Intel's ersure code lib - ISA-L](https://01.org/intel®-storage-acceleration-library-open-source-version)
		    * Shared [a benchmark report](https://github.com/leo-project/notes/blob/master/leofs/benchmark/leofs/1.4/erasure_code/20151222_isars_k10m4_15m_r49w1_60min_1/README.md) on Dec 22, 2015
		* Supported [JErasure](https://github.com/tsuraan/Jerasure) - *Not stable*
	* Delivered ``set-redundancy-method`` on [leofs-adm](https://github.com/leo-project/leofs/blob/1.4/leofs-adm#L693) to configure parameters of the erasure-coding of a bucket

	```
	## erasure-coding
	$ leofs-adm set-redundancy-method <bucket> <access-key-id> erasure-code \
                                     <number-of-data-chunks> <number-of-conding-chunks>

	## replication
	$ leofs-adm set-redundancy-method <bucket> <access-key-id> copy	```

* [#367](https://github.com/leo-project/leofs/issues/367) ``s3-client`` s3cmd sync and put/get with -r option is not working
* [#375](https://github.com/leo-project/leofs/issues/375) ``NFS`` Reduce unnecessary round trips between nfs client and leo_gateway
* [#432](https://github.com/leo-project/leofs/issues/432) ``leo_gateway`` Spend much time on generating an xml object


### Used Libraries
#### leo project
* [leo_backend-db v1.1.14](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.14)
* [leo_cache 1.4-branch](https://github.com/leo-project/leo_cache/commit/76e9fcded4ae4b5b2fa524a099dcfeb3ebfb6e67)
* [leo_commons v1.1.6](https://github.com/leo-project/leo_commons/releases/tag/1.1.6)
* [leo_dcerl v0.4.1](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.1)
* [leo_logger v1.2.1](https://github.com/leo-project/leo_logger/releases/tag/1.2.1)
* [leo_mcerl v0.6.0](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.0)
* [leo_mq v1.3.17](https://github.com/leo-project/leo_mq/releases/tag/1.3.17)
* [leo_object_storage 1.4-branch](https://github.com/leo-project/leo_object_storage/commit/178b55274febabbd9b4f12e4c2ba3b21cab3eaa1)
* [leo_ordning_reda v1.1.4](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.1.4)
* [leo_redundant_manager 1.9.21](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.21)
* [leo_rpc v0.10.7](https://github.com/leo-project/leo_rpc/releases/tag/0.10.7)
* [leo_pod v0.6.4](https://github.com/leo-project/leo_pod/releases/tag/0.6.4)
* [leo_s3_libs v1.2.4](https://github.com/leo-project/leo_s3_libs/releases/tag/1.2.4)
* [leo_statistics v1.1.10](https://github.com/leo-project/leo_statistics/releases/tag/1.1.10)
* [leo_tran v0.2.4](https://github.com/leo-project/leo_tran/releases/tag/0.2.4)
* [leo_watchdog v0.12.1](https://github.com/leo-project/leo_watchdog/releases/tag/0.12.1)
* [savanna_agent v0.4.13](https://github.com/leo-project/savanna_agent/releases/tag/0.4.13)
* [savanna_commons v0.8.16](https://github.com/leo-project/savanna_commons/releases/tag/0.8.16)
* [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
* [nfs_rpc_server v0.2.3](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.3)
* [leo_gateway v1.4.0-pre.3](https://github.com/leo-project/leo_gateway/releases/tag/1.4.0-pre.3)
* [leo_manager v1.4.0-pre.3](https://github.com/leo-project/leo_manager/releases/tag/1.4.0-pre.3)
* [leo_storage v1.4.0-pre.3](https://github.com/leo-project/leo_storage/releases/tag/1.4.0-pre.3)

### others
* [bitcask v2.0.1](https://github.com/lbasho/bitcask/releases/tag/2.0.1)
* [cowboy v1.0.0](https://github.com/leo-project/cowboy/releases/tag/1.0.0-p1)
* [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
* [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
* [eleveldb v2.1.4](https://github.com/basho/eleveldb/releases/tag/2.1.4)
* [folsom v0.8.2-for-leofs](https://github.com/leo-project/folsom/releases/tag/0.8.2-for-leofs)
* [jiffy v0.14.4](https://github.com/davisp/jiffy/releases/tag/0.14.4)
* [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2)
* [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


## 1.4.0-pre.2
### Features and Improvements for LeoFS
* NFS Support
	* NFS Support for FreeBSD
	* Implement NFSPROC3_READDIR based on NFSPROC3_READDIRPLUS
	* Handle Deleted Sub-Directories in is_empty_dir/1
* Improved the ``ls`` command support
	* Able to remove a directory and objects from the dir-metadata with synchronous
* [#325](https://github.com/leo-project/leofs/issues/325) ``leo_gateway`` Reduce network traffic between leo_gateway and leo_storage

### Used Libraries
#### leo project
* [leo_backend-db v1.1.14](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.14)
* [leo_cache 1.4-branch](https://github.com/leo-project/leo_cache/tree/1.4)
* [leo_commons v1.1.6](https://github.com/leo-project/leo_commons/releases/tag/1.1.6)
* [leo_dcerl v0.4.1](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.1)
* [leo_logger v1.2.1](https://github.com/leo-project/leo_logger/releases/tag/1.2.1)
* [leo_mcerl v0.6.0](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.0)
* [leo_mq v1.3.17](https://github.com/leo-project/leo_mq/releases/tag/1.3.17)
* [leo_object_storage 1.4-branch](https://github.com/leo-project/leo_object_storage/tree/1.4)
* [leo_ordning_reda v1.1.4](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.1.4)
* [leo_redundant_manager 1.9.20](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.20)
* [leo_rpc v0.10.7](https://github.com/leo-project/leo_rpc/releases/tag/0.10.7)
* [leo_pod v0.6.4](https://github.com/leo-project/leo_pod/releases/tag/0.6.4)
* [leo_s3_libs v1.2.3](https://github.com/leo-project/leo_s3_libs/releases/tag/1.2.3)
* [leo_statistics v1.1.10](https://github.com/leo-project/leo_statistics/releases/tag/1.1.10)
* [leo_watchdog v0.12.1](https://github.com/leo-project/leo_watchdog/releases/tag/0.12.1)
* [savanna_agent v0.4.13](https://github.com/leo-project/savanna_agent/releases/tag/0.4.13)
* [savanna_commons v0.8.16](https://github.com/leo-project/savanna_commons/releases/tag/0.8.16)
* [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
* [nfs_rpc_server v0.2.3](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.3)
* [leo_gateway v1.4.0-pre.2](https://github.com/leo-project/leo_gateway/releases/tag/1.4.0-pre.2)
* [leo_manager v1.4.0-pre.2](https://github.com/leo-project/leo_manager/releases/tag/1.4.0-pre.2)
* [leo_storage v1.4.0-pre.2](https://github.com/leo-project/leo_storage/releases/tag/1.4.0-pre.2)

#### others
* [bitcask v2.0.1](https://github.com/lbasho/bitcask/releases/tag/2.0.1)
* [cowboy v1.0.0](https://github.com/leo-project/cowboy/releases/tag/1.0.0-p1)
* [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
* [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
* [eleveldb v2.1.4](https://github.com/basho/eleveldb/releases/tag/2.1.4)
* [folsom v0.8.2-for-leofs](https://github.com/leo-project/folsom/releases/tag/0.8.2-for-leofs)
* [jiffy v0.14.4](https://github.com/davisp/jiffy/releases/tag/0.14.4)
* [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2)
* [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)


## 1.4.0-pre.1
### Features and Improvements for LeoFS

* New Features
    * AWS-Signature-v4 Support
        * Reference: [Authenticating Requests (AWS Signature Version 4)](http://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html)
* Improvemens
    * Improved LeoFS's NFS performance
        * Implemented LeoFS directory in [LeoStorage](https://github.com/leo-project/leo_storage) without degrading LeoStorage performance
            * [Configuration](https://github.com/leo-project/leo_storage/blob/1.4.0-pre.1/priv/leo_storage.conf#L68-L75):
                * LeoFS Directory DB's directory: ``directory.db_path``
                * LeoFS Directory DB's container buffer: ``directory.cont_buffer_size``
                * LeoFS Directory DB's container expiration time: ``directory.cont_expiration_time``
        * Retrieving list objects - *the ls and tree comand*
        * Copying objects
    * [#283](https://github.com/leo-project/leofs/issues/283) ``leo_s3_libs`` Authenticating requests(AWS Signature version4) to be implemented
    * [#373](https://github.com/leo-project/leofs/issues/373) ``S3-API`` ``AWS-Signature-v4`` ``leo_gateway`` ``leo_s3_libs`` Support aws-sdk-go
    * [#375](https://github.com/leo-project/leofs/issues/375) ``NFS`` Reduce unnecessary round trips between nfs client and leo_gateway
    * [#400](https://github.com/leo-project/leofs/issues/400) ``all`` Use erlang:(max|min) if possible
    * [#403](https://github.com/leo-project/leofs/issues/403) ``s3-tests`` Increase s3-tests coverage

### Bugs Fixed

* [#370](https://github.com/leo-project/leofs/issues/370) ``s3-api`` ``leo_manager`` ``leo_gateway`` Return wrong http response when handling an invalid bucket format
* [#372](https://github.com/leo-project/leofs/issues/372) ``s3-api`` ``leo_gateway`` Return wrong http response when handling an invalid maxkeys parameter
* [#374](https://github.com/leo-project/leofs/issues/374) ``s3-api`` ``leo_gateway`` Return wrong http response when handling an invalid http headers
* [#401](https://github.com/leo-project/leofs/issues/401) ``leo_storage`` 500 error can occur under heavy load with N=1
* [#405](https://github.com/leo-project/leofs/issues/405) ``leo_object_storage`` Crashing ``leo_object_storage_server`` causes a corresponding leo_backend_db_server inaccessible
* [#406](https://github.com/leo-project/leofs/issues/406) ``leo_mq`` Crashing ``leo_mq_publisher`` causes a corresponding leo_backend_db_server inaccessible
* [#407](https://github.com/leo-project/leofs/issues/407) ``leo_ordning_reda`` ``add_container`` and ``remove_container`` can get into race condition

### Used Libraries
#### leo project
* [leo_backend-db v1.1.11](https://github.com/leo-project/leo_backend_db/releases/tag/1.1.11)
* [leo_cache v0.6.7](https://github.com/leo-project/leo_cache/releases/tag/0.6.7)
* [leo_commons v1.1.4](https://github.com/leo-project/leo_commons/releases/tag/1.1.4)
* [leo_dcerl v0.4.0](https://github.com/leo-project/leo_dcerl/releases/tag/0.4.0)
* [leo_logger v1.1.10](https://github.com/leo-project/leo_logger/releases/tag/1.1.10)
* [leo_mcerl v0.6.0](https://github.com/leo-project/leo_mcerl/releases/tag/0.6.0)
* [leo_mq v1.3.14](https://github.com/leo-project/leo_mq/releases/tag/1.3.14)
* [leo_object_storage v1.3.0](https://github.com/leo-project/leo_object_storage/releases/tag/1.3.0)
* [leo_ordning_reda v1.1.2](https://github.com/leo-project/leo_ordning_reda/releases/tag/1.1.2)
* [leo_redundant_manager 1.9.17](https://github.com/leo-project/leo_redundant_manager/releases/tag/1.9.17)
* [leo_rpc v0.10.5](https://github.com/leo-project/leo_rpc/releases/tag/0.10.5)
* [leo_pod v0.6.4](https://github.com/leo-project/leo_pod/releases/tag/0.6.4)
* [leo_s3_libs v1.2.1](https://github.com/leo-project/leo_s3_libs/releases/tag/1.2.1)
* [leo_statistics v1.1.8](https://github.com/leo-project/leo_statistics/releases/tag/1.1.8)
* [leo_watchdog v0.10.4](https://github.com/leo-project/leo_watchdog/releases/tag/0.10.2)
* [savanna_agent v0.4.11](https://github.com/leo-project/savanna_agent/releases/tag/0.4.11)
* [savanna_commons v0.8.14](https://github.com/leo-project/savanna_commons/releases/tag/0.8.14)
* [erpcgen v0.2.3](https://github.com/leo-project/erpcgen/releases/tag/0.2.3)
* [nfs_rpc_server v0.2.3](https://github.com/leo-project/nfs_rpc_server/releases/tag/0.2.3)
* [leo_gateway v1.4.0-pre1](https://github.com/leo-project/leo_gateway/releases/tag/1.4.0-pre.1)
* [leo_manager v1.4.0-pre1](https://github.com/leo-project/leo_manager/releases/tag/1.4.0-pre.1)
* [leo_storage v1.4.0-pre1](https://github.com/leo-project/leo_storage/releases/tag/1.4.0-pre.1)

#### others
* [bitcask v2.0.0](https://github.com/lbasho/bitcask/releases/tag/2.0.0)
* [cowboy v1.0.0](https://github.com/leo-project/cowboy/releases/tag/1.0.0-p1)
* [cowlib v1.0.0](https://github.com/extend/cowboy/releases/tag/1.0.0)
* [elarm v0.3.0](https://github.com/leo-project/elarm/releases/tag/0.3.0)
* [eleveldb v2.1.1](https://github.com/basho/eleveldb/releases/tag/2.1.1)
* [folsom v0.8.2-for-leofs](https://github.com/leo-project/folsom/releases/tag/0.8.2-for-leofs)
* [jiffy v0.13.3](https://github.com/davisp/jiffy/releases/tag/0.13.3)
* [lz4 v0.2.2](https://github.com/leo-project/erlang-lz4/releases/tag/0.2.2)
* [recon v0.8.5](https://github.com/ferd/recon/releases/tag/2.2.1)