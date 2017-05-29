# leo_manager

[![Build Status](https://secure.travis-ci.org/leo-project/leo_manager.png?branch=develop)](http://travis-ci.org/leo-project/leo_manager)

## Overview

* "leo_manager" is one of the core component of [LeoFS](https://github.com/leo-project/leofs). Main roles are described below.
  * "leo_manager" distribute routing-table (RING) which is always monitored by it.
  * In order to continue operation of the storage system, "leo_manager" always monitor nodes in the cluster.
  * "leo_manager" provide a centralized control function such as ATTACH, START, SUSPEND and RESUME.
*  Detail document is [here](http://leo-project.net/leofs/docs/).
* "leo_manager" uses [rebar](https://github.com/rebar/rebar) build system. Makefile so that simply running "make" at the top level should work.
* "leo_manager" requires [Erlang R16B03-1 or later](http://www.erlang.org/).

## Architecture

LeoFS Manager generates and manages a routing table, which is called RING and based on consistent hashing.

LeoFS Manager always monitors every [LeoFS Storage](https://github.com/leo-project/leo_storage) and [LeoFS Gateway](https://github.com/leo-project/leo_gateway) of status and RING in order to keep running LeoFS and consistency of a RING. And also, it distributes RING to LeoFS Storage and LeoFS Gateway.

![leo-manager-architecture](https://raw.githubusercontent.com/leo-project/leofs/master/docs/assets/leofs-architecture.007.jpg)

In addition, LeoFS Manager provides LeoFS administration commands - [leofs-adm](https://raw.githubusercontent.com/leo-project/leofs/master/leofs-adm) to be able to easily operate LeoFS. LeoFS administration commands already cover entire LeoFS functions.

## Sponsors

LeoProject/LeoFS is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and supported by [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
