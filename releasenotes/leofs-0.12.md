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
-----------

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