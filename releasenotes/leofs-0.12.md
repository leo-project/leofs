leofs-0.12.7
============

Features and Improvements for LeoFS
-----------------------------------

* Improve performances
    * Storage Performance Tuning#6
        * Related libs: [leo_storage, leo_object_storage]
        * Reduced compaction-cost
             * Able to execution of parallel comaction
                 * Get maximum performance by setting a appropriate number corresponding with number of cores
* Improve
    * Gateway:
        * The optimal timeout according to file size set up.
        * Modified default cache mode from 'http' to 'inner'
    * Storage:
        * Modified completion of storage for restriction of file destruction
            * When shutting down storage, in charge of storage-process close files
        * Refactor message-queue functions
            * Unified read-failure's queue and write-failure's queue

Bugs Fixed
-----------

* Storage:
    * Overwrite an object during rebalance
        * Always check the version(clock) of object
    * Fix haystacks reopen correctly when failing compaction
* Manager-Console
    * Crush get-endpoints when no-records
    * Crush get-users when no-records


Used Libraries
---------------

* leo project
    * [leo_commons v0.12.6](https://github.com/leo-project/leo_commons.git)
    * [leo_backend-db v0.12.2](https://github.com/leo-project/leo_backend_db.git)
    * [leo_object_storage v0.12.16](https://github.com/leo-project/leo_object_storage.git)
    * [leo_mq v0.12.2](https://github.com/leo-project/leo_mq.git)
    * [leo_ordning_reda v0.8.5](https://github.com/leo-project/leo_ordning_reda.git)
    * [leo_redundant_manager v0.12.4](https://github.com/leo-project/leo_redundant_manager.git)
    * [leo_s3_libs v0.12.2](https://github.com/leo-project/leo_s3_libs.git)
    * [leo_statistics v0.10.6](https://github.com/leo-project/leo_statistics.git)
    * [leo_logger v0.10.3](https://github.com/leo-project/leo_logger.git)
    * [leo_gateway v0.12.9](https://github.com/leo-project/leo_gateway.git)
    * [leo_manager v0.12.9](https://github.com/leo-project/leo_manager.git)
    * [leo_storage v0.12.9](https://github.com/leo-project/leo_storage.git)
    * [ecache v0.10.5](https://github.com/leo-project/ecache.git)
    * [cherly v0.12.0](https://github.com/leo-project/cherly.git)
* others
    * [bear](htts://github.com/boundary/bear.git)
    * [bitcask](https://github.com/basho/bitcask.git)
    * [cowboy v0.6.2](https://github.com/leo-project/cowboy.git) - forked from [extend/cowboy](https://github.com/extend/cowboy)
    * [folsom](https://github.com/boundary/folsom.git)
    * [jiffy](https://github.com/davisp/jiffy.git)
    * [lz4 v0.1.1](https://github.com/leo-project/erlang-lz4.git) - forked from [szktty/erlang-lz4](https://github.com/szktty/erlng-lz4)


leofs-0.12.5
============

Features and Improvements for LeoFS
-----------------------------------

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

Bugs Fixed
-----------

* Respond invalid "Etag" from the gateway when using multipart-upload API.
* Possibility of file-destruction
    * Termination of storages for the restriction of file-destruction
        * Related libs: [leo_storage, leo_object_storage]

Used Libraries
---------------

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


leofs-0.12.4
============

Features and Improvements for LeoFS
-----------------------------------

* Improve performances
    * Storage Performance Tuning#4
        * Target libs: [leo_storage, leo_object_storage]
        * Reduced using spawn/1 in replicator
        * Used rpc:cast/4 for async-replication
* Improve
    * Able to retrieve leofs-uesrs from manager-console
    * NOT allow duplication registration of a user-account into the s3-credential

Bugs Fixed
-----------

* Fix bugs
    * Fail rebalance and compaction when exists chunked objects in the storage
    * Manager-console crashed when inputed invalid parameter(s)


Used Libraries
---------------

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


leofs-0.12.3
============

Features and Improvements for LeoFS
-----------------------------------

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

Bugs Fixed
-----------

* Fix bugs
    * Deletion of chunked objects (large-object)
    * Fail rebalance when exists a restarting node
        * Adjust the start timing of RPC for reject requests from remote-node(s)

Used Libraries
---------------

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


leofs-0.12.2
============

Features and Improvements for LeoFS
-----------------------------------

* Improve performances
    * Storage Performance Tuning#2
        * Revised put operation
* Improve S3 compatibility
    * Support deletion of a bucket
    * Support [s3cmd](http://s3tools.org/s3cmd) (s3-client)

Bugs Fixed
-----------

* Fix bugs
    * Gateway:
        * NOT able to object-cache because data-type is replaced from string to binary with 0.12.1
    * Storage "Compaction"
        * When excuting compact-command, Objects to be removed remain (chunked objects)

Used Libraries
---------------

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


leofs-0.12.1
============

Features and Improvements for LeoFS
-----------------------------------

* Improve performances
    * Storage Performance Tuning#1
        * Reduced overhead
            * Modified replicator/repairer from gen_server to module
            * Revised data-flow from storage-engine to object/metadata storage
    * Reduced using list_to_binary/1
        * Modified buecket-related libs: [leo_gateway,leo_manager,leo_s3_libs]
    * Compressor/Decompressor replace from snappy to lz4

Bugs Fixed
-----------

* Fix bugs
    * Has omissions an object of rebalance
    * Fixed S3 releated:
        * NOT able to remove an endpoint
        * Able to add a bucket with NOT exists access-key

Used Libraries
---------------

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



leofs-0.12.0
============

Features and Improvements for LeoFS
-----------------------------------

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

Bugs Fixed
-----------

* S3-API related
    * Overwrited bucket-info by NOT owners
    * When put-operation, NOT returned 'ETag' header
* Compaction
    * When excuting compact-command, Objects to be removed partly may remain

Used Libraries
---------------

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
