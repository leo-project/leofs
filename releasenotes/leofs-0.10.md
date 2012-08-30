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
        * [s3-get-buckets](http://www.leofs.org/docs/admin_guide.html#s3-get-buckets-retrieve-all-of-buckets-registered) : Retrieve all of Buckets registered


* Improve order of system launch
    * Remove "attach command" in manager - After Storage launched, the node's state is automatically changed to "attached"
* Improve rebalance-function's performance which is about 5 times compare with v0.9.1

Bugs Fixed
-----------

* Deletion of Zero bytes in Storage
* Behavior after the restart of Manager
* Re-register procs into the Manager's monitor

Known Issues
-------------

* [DU comand](http://www.leofs.org/docs/admin_guide.html#du-retrieve-a-number-of-objects-from-object-storage) and [Compact command](http://www.leofs.org/docs/admin_guide.html#compact-remove-logical-deleted-objects-and-metadata-from-object-storage-and-metadata-storage-respectively) are NOT stable. We will resolve them with v0.10.1

