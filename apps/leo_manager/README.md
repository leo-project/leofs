# leo_manager

## Overview

* "leo_manager" is one of the core components of [LeoFS](https://github.com/leo-project/leofs). Main roles are described below.
  * "leo_manager" distributes routing-table (RING) which is always monitored by it.
  * In order to continue operation of the storage system, "leo_manager" always monitors nodes in the cluster.
  * "leo_manager" provides a centralized control function such as ATTACH, START, SUSPEND and RESUME.
* Detail document is [here](http://leo-project.net/leofs/docs/).
* "leo_manager" uses [rebar3](https://github.com/erlang/rebar3) build system. Makefile so that simply running "make" at the top level should work.
* "leo_manager" requires [Erlang/OTP 28 or later](http://www.erlang.org/).

## Build

```bash
$ make compile
```

## Architecture

LeoFS Manager generates and manages a routing table, which is called RING and based on consistent hashing.

LeoFS Manager always monitors every [LeoFS Storage](https://github.com/leo-project/leofs/apps/leo_storage) and [LeoFS Gateway](https://github.com/leo-project/leofs/apps/leo_gateway) of status and RING in order to keep running LeoFS and consistency of a RING. And also, it distributes RING to LeoFS Storage and LeoFS Gateway.

![leo-manager-architecture](https://raw.githubusercontent.com/leo-project/leofs/master/docs/assets/leofs-architecture.007.jpg)

In addition, LeoFS Manager provides LeoFS administration commands - [leofs-adm](https://raw.githubusercontent.com/leo-project/leofs/master/leofs-adm) to be able to easily operate LeoFS. LeoFS administration commands already cover entire LeoFS functions.

## Sponsors

* LeoProject/LeoFS is sponsored by [Lions Data, Ltd.](https://lions-data.com/) from Jan of 2019.
* LeoProject/LeoFS was sponsored by [Rakuten, Inc.](https://global.rakuten.com/corp/) from 2012 to Dec of 2018.

