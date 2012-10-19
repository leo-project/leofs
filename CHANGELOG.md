CHANGELOG
=========

0.12.0
-------

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
* Used version of libs
    * leo_commons : 0.12.0
    * leo_backend-db : 0.10.4
    * leo_object_storage : 0.12.3
    * leo_mq : 0.10.2
    * leo_ordning_reda : 0.6.1
    * leo_redundant_manager : 0.10.2
    * leo_s3_libs : 0.10.2
    * leo_statistics : 0.10.1
    * leo_logger : 0.9.7
    * ecache : 0.10.1
    * cherly : 0.10.0
    * cowboy : 0.6.2


0.10.2
-------

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


0.10.1
-------

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


0.10.0
-------

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


0.9.1
-----

* Remove "apps" directory in leofs - Modified "reltool.config"
* Fix 'shadow vars'
* Remove ZMQ log-appender and AMQP log-appender for leo_logger, They will be provided LeoFS's sub projects.
* Improve - In order to be able to extend "LeoFS's Object Container's file format".


0.9.0
-----

* Initial release
