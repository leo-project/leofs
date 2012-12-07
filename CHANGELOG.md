CHANGELOG
=========

0.12.5 (Dec 7, 2012)
---------------------

* Improve performances
    * Storage Performance Tuning#5
        * Related libs: [leo_storage, leo_object_storage]
        * Reduced using spawn/1 in replicator #2
* Improve
    * Able to monitor VM-values on Leo Manager's console
    * Deprecate parameterized module in leo_object_storage
        * Reference: http://www.erlang.org/news/35
    * Modified re-launch storage process
        * When regularly stop the storage-process, It writes current status in a file. Then it restarts the storage-process when reading the file.
    * Able to post a large part of an object to LeoFS with multipart-upload API.
* Fix bugs
    * Respond invalid "Etag" from the gateway when using multipart-upload API.
    * Possibility of file-destruction
        * Termination of storages for the restriction of file-destruction
            * Related libs: [leo_storage, leo_object_storage]

* Used libraries
    * leo project
        * [leo_commons v0.12.6](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.12.0](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.12](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.12.0](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.4](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.12.1](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.12.0](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.6](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.10.2](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.12.6](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.12.6](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.12.6](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.3](https://github.com/leo-project/ecache.git)
        * [cherly v0.10.2](https://github.com/leo-project/cherly.git)
    * others
        * [bear](htts://github.com/boundary/bear.git)
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlang-lz4)


0.12.4 (Nov 21, 2012)
---------------------

* Improve performances
    * Storage Performance Tuning#4
        * Target libs: [leo_storage, leo_object_storage]
        * Reduced using spawn/1 in replicator #1
        * Used rpc:cast/4 for async-replication
* Improve
    * Able to retrieve leofs-uesrs from manager-console
    * NOT allow duplication registration of a user-account into the s3-credential
* Fix bugs
    * Fail rebalance and compaction when exists chunked objects in the storage
    * Manager-console crashed when inputed invalid parameter(s)

* Used libraries
    * leo project
        * [leo_commons v0.12.5](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.10.8](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.11](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.10.5](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.3](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.12.0](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.10.7](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.5](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.10.1](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.12.5](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.12.5](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.12.5](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.2](https://github.com/leo-project/ecache.git)
        * [cherly v0.10.1](https://github.com/leo-project/cherly.git)
    * others
        * [bear](htts://github.com/boundary/bear.git)
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlang-lz4)


0.12.3 (Nov 9, 2012)
--------------------

* Improve performances
    * Storage Performance Tuning#3
        * Reduced using list_to_binary/1 - [leo_storage, leo_object_storage]
        * Unified internal storage data-type to "binary"
* Improve S3 compatibility
    * Support Multipart upload an object (large-object)
* New feature
    * Able to monitor (SNMP)
        * Sum of objects/storage-node
        * Sum of object-length/storage-node
* Fix bugs
    * Deletion of chunked objects (large-object)
    * Fail rebalance when exists a restarting node
        * Adjust the start timing of RPC for reject requests from remote-node(s)

* Used libraries
    * leo project
        * [leo_commons v0.12.5](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.10.8](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.10](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.10.5](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.3](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.12.0](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.10.6](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.5](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.10.1](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.12.4](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.12.4](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.12.4](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.1](https://github.com/leo-project/ecache.git)
        * [cherly v0.10.0](https://github.com/leo-project/cherly.git)
    * others
        * [bear](htts://github.com/boundary/bear.git)
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlang-lz4)


0.12.2 (Nov 2, 2012)
--------------------

* Improve performances
    * Storage Performance Tuning#2
        * Revised put operation
* Improve S3 compatibility
    * Support deletion of a bucket
    * Support [s3cmd](http://s3tools.org/s3cmd) (s3-client)
* Fix bugs
    * Gateway:
        * NOT able to store object-cache because data-type is replaced from string to binary with 0.12.1
    * Storage "Compaction"
        * When excuting compact-command, Objects to be removed remain (chunked objects)
* Used libraries
    * leo project
        * [leo_commons v0.12.3](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.10.7](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.9](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.10.4](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.2](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.10.4](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.10.5](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.4](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.10.0](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.12.3](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.12.3](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.12.3](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.1](https://github.com/leo-project/ecache.git)
        * [cherly v0.10.0](https://github.com/leo-project/cherly.git)
    * others
        * [bear](htts://github.com/boundary/bear.git)
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlang-lz4)


0.12.1  (Oct 25, 2012)
----------------------

* Improve performances
    * Storage Performance Tuning#1
        * Reduced overhead
            * Modified replicator/repairer from gen_server to module
            * Revised data-flow from storage-engine to object/metadata storage
    * Reduced using list_to_binary/1
        * Modified bucket-related libs: [leo_gateway,leo_manager,leo_s3_libs]
    * Compressor/Decompressor replace from snappy to lz4
* Fix bugs
    * Has omissions an object of rebalance
    * Fixed S3 releated:
        * NOT able to remove an endpoint
        * Able to add a bucket with NOT exists access-key

* Used libraries
    * leo project
        * [leo_commons v0.12.0](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.10.4](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.4](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.10.2](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.8.0](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.10.2](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.10.3](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.2](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.9.8](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.12.1](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.12.2](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.12.1](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.1](https://github.com/leo-project/ecache.git)
        * [cherly v0.10.0](https://github.com/leo-project/cherly.git)
    * others
        * [bear](htts://github.com/boundary/bear.git)
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlang-lz4)


0.12.0 (Oct 20, 2012)
----------------------

* New feature - Large Object Support
    * Handled from a few bytes an object to a few GB an object
* Improve performances
    * Gateway Performance Tuning
        * HTTP-Server replace from Mochiweb to Cowboy
        * Reduced using list_to_binary/1
    * Revised order of system launch
        * before: Managers -> Storage -> Gateway
        * after : Managers -> Storage|Gateway
    * Changed type of key from string to binary
* Fix bugs
    * S3-API related
        * Overwrited bucket-info by NOT owners
        * When put-operation, NOT returned 'ETag' header
    * Compaction
        * When excuting compact-command, Objects to be removed partly may remain
* Used libraries
    * leo project
        * [leo_commons v0.12.0](https://github.com/leo-project/leo_commons.git)
        * [leo_backend-db v0.10.4](https://github.com/leo-project/leo_backend_db.git)
        * [leo_object_storage v0.12.3](https://github.com/leo-project/leo_object_storage.git)
        * [leo_mq v0.10.2](https://github.com/leo-project/leo_mq.git)
        * [leo_ordning_reda v0.6.1](https://github.com/leo-project/leo_ordning_reda.git)
        * [leo_redundant_manager v0.10.2](https://github.com/leo-project/leo_redundant_manager.git)
        * [leo_s3_libs v0.10.2](https://github.com/leo-project/leo_s3_libs.git)
        * [leo_statistics v0.10.1](https://github.com/leo-project/leo_statistics.git)
        * [leo_logger v0.9.7](https://github.com/leo-project/leo_logger.git)
        * [leo_gateway v0.12.0](https://github.com/leo-project/leo_gateway.git)
        * [leo_manager v0.12.0](https://github.com/leo-project/leo_manager.git)
        * [leo_storage v0.12.0](https://github.com/leo-project/leo_storage.git)
        * [ecache v0.10.1](https://github.com/leo-project/ecache.git)
        * [cherly v0.10.0](https://github.com/leo-project/cherly.git)
    * others
        * [bear](htts://github.com/boundary/bear.git)
        * [bitcask](https://github.com/basho/bitcask.git)
        * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
        * [folsom](https://github.com/boundary/folsom.git)
        * [jiffy](https://github.com/davisp/jiffy.git)
        * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlang-lz4)
        * [snappy](https://github.com/fdmanana/snappy-erlang-nif.git)


0.10.2 (Sep 25, 2012)
----------------------

* Improve performances
    * NOT used "proplists:get_value" function
        * Replace from "proplists:get_value/2,3" to "lists:keyfind/2"
    * Related libs:
        * leo_gateway
        * leo_storage
        * leo_manager
        * leo_logger
        * leo_mq
        * leo_object_storage
        * leo_ordning_reda
        * leo_redundant_manager
        * leo_s3_libs
        * leo_statistics
* Improve leo_manager
    * Support output of json-format
    * Support multi-ports TCP-server
        * for CUI console
        * for Application (JSON-Format)
* Improve leo_storage
    * Support plural devices
    * Reduced rebalance/recover costs
        * Support compression of stacked objects and decompresson of received objects (Using snappy)
* Fix bugs
    * leo-gateway related (S3-related):
        * Create bucket from 'Dragon Disk'
        * When using cowboy can send bad values(not iodata)


0.10.1 (Sep 12, 2012)
---------------------

* Improve leo_hex performances
    * "binary_to_hex" function
    * Related libs:
        * leo_gateway
        * leo_object_storage
        * leo_redundant_manager
    * By this correspondence, LeoFS's performance improved 20% up.
* Improve leo_manager
    * Format of output from manager-console
        * Commands:
            * "status"
            * "whereis"
* Improve performance of "storage-stats" in manager-console
* Fix bugs
    * A handling error in S3-libs


0.10.0 (Aug 30, 2012)
---------------------

* Improve S3-API's compatibility
    * Add S3-authentication
    * Add S3-bucket
* Add S3-related command in LeoFS's manager
    * "s3-gen-key" : Generate a S3 key pair(AccessKeyID and SecretAccessKey)
    * "s3-set-endpoint" : Register a new S3 Endpoint
    * "s3-delete-endpoint" : Delete a S3 Endpoint
    * "s3-get-endpoints" : Retrieve all of S3 Endpoints registered
    * "s3-get-buckets" : Retrieve all of Buckets registered
* Improve order of system launch
    * Remove "attach command" in manager - After Storage launched, the node's state is automatically changed to "attached"
* Improve rebalance-function's performance which is about 5 times compare with v0.9.1
* Improve compact-function. Restrain storage's load when compact objects.
* Fix bugs
    * Deletion of Zero bytes in Storage
    * Behavior after the restart of Manager
    * Re-register procs into the Manager's monitor


0.9.1 (Jul 13, 2012)
--------------------

* Remove "apps" directory in leofs - Modified "reltool.config"
* Fix 'shadow vars'
* Remove ZMQ log-appender and AMQP log-appender for leo_logger, They will be provided LeoFS's sub projects.
* Improve - In order to be able to extend "LeoFS's Object Container's file format".


0.9.0  (Jul 4, 2012)
--------------------

* Initial release
