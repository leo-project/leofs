leofs-0.9.0
===========
The initial public release of LeoFS.


leofs-0.9.1
===========

Improvements
-------------

* Removed ZMQ log-appender and AMQP log-appender for leo_logger, They will be provided LeoFS's sub projects.
* Updated "LeoFS's object container's header", because of be able to add items in it. So, It's NOT compatibility of 0.9.0 and 0.9.1. But from this version later, We will consider its compatiblity.

Bugs Fixed
-----------

* Removed 'apps' directory in leofs - Modified 'reltool.config'
* Fixed 'shadow vars'


Known Issues
-------------

* [DU comand](http://www.leofs.org/docs/admin_guide.html#du-retrieve-a-number-of-objects-from-object-storage) and [Compact command](http://www.leofs.org/docs/admin_guide.html#compact-remove-logical-deleted-objects-and-metadata-from-object-storage-and-metadata-storage-respectively) are NOT stable. We will resolve them with next version.

