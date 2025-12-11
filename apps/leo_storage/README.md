# leo_storage

## Overview

* "leo_storage" is one of the core components of [LeoFS](https://github.com/leo-project/leofs). Main roles are described below.
  * "leo_storage" is log structure file system for Object/BLOB. Also, it includes metadata-server which is powered by [eleveldb](https://github.com/basho/eleveldb).
  * "leo_storage" is master-less; It has NO-SPOF.
  * LeoFS's storage-cluster consists of a set of loosely connected nodes.
* "leo_storage" uses [rebar3](https://github.com/erlang/rebar3) build system. Makefile so that simply running "make" at the top level should work.
* "leo_storage" requires Erlang/OTP 28 or later.

## Build

```bash
$ make
```

## Test

```bash
$ make eunit
```

## Architecture

### Fundamentals

LeoFS Storage consists of object and the metadata storage. In addition, it includes replicator and repairer in order to realise Eventual consistency.

![leo-storage-architecture](https://raw.githubusercontent.com/leo-project/leofs/master/docs/assets/leofs-architecture.003.jpg)

In case of a write operation, LeoFS Storage accepts a request from LeoFS Gateway then automatically replicate an object into the LeoFS Storage cluster. Finally, LeoFS Storage confirms whether a stored object satisfy the consistency rule or NOT.

On the other hand, in case of a read operation, LeoFS Gateway requests a LeoFS Storage node. Then the LeoFS Storage node retrieves an object from the local object-storage or the remote LeoFS Storage node. Finaly, the LeoFS Storage node respond an object to the gateway. Also, the LeoFS Storage node checks consistency of the object with the asynchronous processing.

If the LeoFS Storage node finds inconsistency of an object, it will be recovered with the backend process. The object eventually keep consistensy with their functions.

### Data Structure

LeoFS's object consists of 3 layers which are metadata, needle and object container.

![leo-storage-data-structure](https://raw.githubusercontent.com/leo-project/leofs/master/docs/assets/leofs-architecture.005.jpg)

* The object storage manages and stores both an object and a metadata, which merges as a needle.
* The metadata storage manages and stores attributes of an object which includes filename, size, checksum, and so on. And it depends of bitcask or leveldb.
* The object container is a log structured file format.
  * This format is robust and high performance because effect of local file system is just a little part.
  * LeoFS Storage is necessary to GC - the compaction mechanism in order to remove unnecessary objects from the object container.

### Large Size Object Support

![leo-storage-large-object-support](https://raw.githubusercontent.com/leo-project/leofs/master/docs/assets/leofs-architecture.006.jpg)

LeoFS supports to handle a large size object since v0.12. The purpose of this function is 2 things:

* 1st one is to equalize disk usage of every LeoFS Storage node.
* 2nd one is to realize high I/O efficiency and high availability.

In case of a write operation, a large size object is divided to plural objects at LeoFS Gateway then they're replicated into the LeoFS Storage cluster similarly to a small size object. And also, the default chunk size is 5 mega bytes, value of which is able to change a custom chunked object size.

On the other hand, In case of READ of a large object, first, LeoFS Gateway retrieves a metadata of a requested object from a client. Then if it is a large size object, LeoFS Gateway retrieves the chunked objects in order of the chunk object number from the LeoFS Storage cluster. Finally, LeoFS Gateway responds the objects to the client.

## Dependencies

| Library | Version |
|---------|---------|
| [leo_commons](https://github.com/leo-project/leo_commons) | 1.3.0 |
| [leo_mq](https://github.com/leo-project/leo_mq) | v2.1.0 |
| [leo_object_storage](https://github.com/leo-project/leo_object_storage) | v2.1.0 |
| [leo_ordning_reda](https://github.com/leo-project/leo_ordning_reda) | v1.3.0 |
| [leo_redundant_manager](https://github.com/leo-project/leo_redundant_manager) | v2.1.0 |
| [leo_statistics](https://github.com/leo-project/leo_statistics) | v2.1.0 |
| [leo_watchdog](https://github.com/leo-project/leo_watchdog) | v2.1.0 |
| [sd_notify](https://github.com/systemd/erlang-sd_notify) | v1.1 |

## License

leo_storage's license is [Apache License Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html).

## Sponsors

* LeoProject/LeoFS was sponsored by [Rakuten, Inc.](https://global.rakuten.com/corp/) from 2012 to Dec of 2018.
* LeoProject/LeoFS is sponsored by [Lions Data, Ltd.](https://lions-data.com/) from Jan of 2019.

----

Copyright (c) 2019-2025 Lions Data, Ltd.
