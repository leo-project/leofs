leofs-0.16.0
==============

Features and Improvements for LeoFS
-----------------------------------

* New Features
    * Gateway
        * Supported GET/PUT ACL a Bucket [S3-API]
        * Supported Exists a Bucket [S3-API]
        * (Beta) Able to output access-logs to Elasticsearch in order to realize to access-log analysis with Kibana
    * Other
        * Implemented SmartOS packager (Contributed by Heinz N. Gies - [Project FiFo](http://project-fifo.net/))
* Improved
    * Manager
        * Supported single 'Manager'
        * Supported to check # of detached node before executing detach-command

Used Libraries
---------------

* leo project
    * [leo_backend-db v0.14.1](https://github.com/leo-project/leo_backend_db.git)
    * [leo_cache v0.4.13](https://github.com/leo-project/leo_cache.git)
    * [leo_commons v0.14.8](https://github.com/leo-project/leo_commons.git)
    * [leo_dcerl v0.2.4](https://github.com/leo-project/leo_dcerl.git)
    * [leo_logger v0.12.5](https://github.com/leo-project/leo_logger.git)
    * [leo_mcerl v0.2.6](https://github.com/leo-project/leo_mcerl.git)
    * [leo_mq v0.12.20](https://github.com/leo-project/leo_mq.git)
    * [leo_object_storage v0.14.7](https://github.com/leo-project/leo_object_storage.git)
    * [leo_ordning_reda v0.8.17](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager v1.0.4](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_s3_libs v0.12.17](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v0.14.6](https://github.com/leo-project/leo_statistics.git)
    * [leo_gateway v0.16.0](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v0.16.0](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v0.16.0](https://github.com/leo-project/leo_storage.git)
* others
    * [bitcask v1.6.2](https://github.com/basho/bitcask.git)
    * [cowboy v0.8.6](https://github.com/extend/cowboy.git)
    * [folsom](https://github.com/boundary/folsom.git)
    * [jiffy](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)
