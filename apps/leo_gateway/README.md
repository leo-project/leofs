# leo_gateway

[![Build Status](https://secure.travis-ci.org/leo-project/leo_gateway.png?branch=develop)](http://travis-ci.org/leo-project/leo_gateway)

## Overview

* **leo_gateway**, LeoGateway is one of the core components of [LeoFS](https://github.com/leo-project/leofs). Main roles are described below.
  * LeoFS's Gateway use [Cowboy](https://github.com/extend/cowboy) as Erlang's HTTP server.
  * Able to speak [Amazon S3 compatible REST API](http://docs.amazonwebservices.com/AmazonS3/2006-03-01/dev/Welcome.html?r=5754).
  * Implemented a subset of Caching in HTTP(RFC2616).

## Architecture

LeoFS Gateway consists of the fast HTTP-Server - [Cowboy](https://github.com/ninenines/cowboy), the API handler and [the cache mecanism](https://github.com/leo-project/leo_cache). It provides the REST-API and Amazon S3-API. You’re able to easily access LeoFS with S3-Clients such as `s3cmd`, `DragonDisk`, program languages - `Erlang`, `Java`, `Ruby`, `Python`, `Go` and so on.

![leo_gateway_architecture](http://leo-project.net/leofs/docs/_images/leofs-architecture.002.jpg)

A client requests an object or a bucket operation to LeoFS Gateway then LeoFS Gateway requests the message of operation to a storage-node.

A destination storage node is decided by the routing-table. It is called RING which is generated and provided at LeoFS Manager and which is based on consistent-hashing.

Also, LeoFS Gateway provides built-in support for the object-cache mechanism in order to realize Keeping high performance and reduction of traffic between LeoFS Gateway and [LeoFS Storage](https://github.com/leo-project/leo_storage).

* The detail document is [here](http://leo-project.net/leofs/docs/).
* `leo_gateway` uses [rebar](https://github.com/rebar/rebar) build system. Makefile so that simply running "make" at the top level should work.
* `leo_gateway` requires [Erlang R16B03-1 or later](http://www.erlang.org/).

## Sponsors

LeoProject/LeoFS is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and supported by [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
