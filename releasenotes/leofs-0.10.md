leofs-0.10.2
============

Features and Improvements for LeoFS
-----------------------------------

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

Bugs Fixed
-----------

* leo-gateway related (S3-related):
    * Create bucket from 'Dragon Disk'
    * When using cowboy can send bad values(not iodata)


leofs-0.10.1
============

Features and Improvements for LeoFS
-----------------------------------

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

Bugs Fixed
-----------

* Fixed a handling error in S3-libs



leofs-0.10.0
============

Features and Improvements for LeoFS
-----------------------------------

* Improve S3-API's compatibility
    * Add S3-authentication
    * Add S3-bucket
* Add S3-related command in LeoFS's manager
    * Document is [here](http://www.leofs.org/docs/admin_guide.html).
    * Commands:
        * [s3-gen-key](http://www.leofs.org/docs/admin_guide.html#s3-gen-key-generate-a-s3-key-pair-accesskeyid-and-secretaccesskey) : Generate a S3 key pair(AccessKeyID and SecretAccessKey)
        * [s3-set-endpoint](http://www.leofs.org/docs/admin_guide.html#s3-set-endpoint-register-a-new-s3-endpoint) : Register a new S3 Endpoint
        * [s3-delete-endpoint](http://www.leofs.org/docs/admin_guide.html#s3-delete-endpoint-delete-a-s3-endpoint) : Delete a S3 Endpoint
        * [s3-get-endpoints](http://www.leofs.org/docs/admin_guide.html#s3-get-endpoints-retrieve-all-of-s3-endpoints-registered) : Retrieve all of S3 Endpoints registered
        * [s3-get-buckets](http://www.leofs.org/docs/admin_guide.html#s3-get-buckets-retrieve-all-of-buckets-registered) : Retrieve all of Buckets registered.
* Improve order of system launch
    * Remove "attach command" in manager - After Storage launched, the node's state is automatically changed to "attached".
* Improve rebalance-function's performance which is about 5 times compare with v0.9.1.
* Improve compact-function. Restrain storage's load when compacting objects.

Bugs Fixed
-----------

* Deletion of Zero bytes in Storage
* Behavior after the restart of Manager
* Re-register procs into the Manager's monitor

Known Issues
-------------

* [DU comand](http://www.leofs.org/docs/admin_guide.html#du-retrieve-a-number-of-objects-from-object-storage) is NOT stable. We will resolve it with v0.10.1

