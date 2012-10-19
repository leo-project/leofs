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

Libraries
-----------
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
