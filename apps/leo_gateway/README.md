# leo_gateway
## Overview

* **leo_gateway**, LeoGateway is one of the core components of [LeoFS](https://github.com/leo-project/leofs). Main roles are described below.
  * LeoFS's Gateway uses [Cowboy 2.x](https://github.com/ninenines/cowboy) as Erlang's HTTP server.
  * Implemented a subset of Caching in HTTP(RFC2616).

## Architecture

LeoFS Gateway consists of the fast HTTP-Server - [Cowboy 2.x](https://github.com/ninenines/cowboy), the API handler and [the cache mechanism](https://github.com/leo-project/leo_cache). It provides the REST-API and Amazon S3-API. You're able to easily access LeoFS with S3-Clients such as `s3cmd`, `DragonDisk`, program languages - `Erlang`, `Java`, `Ruby`, `Python`, `Go` and so on.

![leo_gateway_architecture](https://raw.githubusercontent.com/leo-project/leofs/master/docs/assets/leofs-architecture.002.jpg)

A client requests an object or a bucket operation to LeoFS Gateway then LeoFS Gateway requests the message of operation to a storage-node.

A destination storage node is decided by the routing-table. It is called RING which is generated and provided at LeoFS Manager and which is based on consistent-hashing.

Also, LeoFS Gateway provides built-in support for the object-cache mechanism in order to realize Keeping high performance and reduction of traffic between LeoFS Gateway and [LeoFS Storage](https://github.com/leo-project/apps/leo_storage).

## Features

### HTTP Range Requests (RFC 7233)

LeoFS Gateway supports HTTP Range requests for partial content retrieval:

* Normal ranges: `bytes=0-499` (first 500 bytes)
* Open-ended ranges: `bytes=500-` (from byte 500 to end)
* Suffix ranges: `bytes=-500` (last 500 bytes)

### AWS Signature Version 4 Chunked Upload

LeoFS Gateway supports AWS Signature Version 4 chunked upload with per-chunk signature verification for secure streaming uploads.

## Build

* The detail document is [here](http://leo-project.net/leofs/docs/).
* `leo_gateway` uses [rebar3](https://github.com/erlang/rebar3) build system. Makefile so that simply running "make" at the top level should work.
* `leo_gateway` requires Erlang/OTP 24 or later.

## Sponsors

* LeoProject/LeoFS is sponsored by [Lions Data, Ltd.](https://lions-data.com/) from Jan of 2019.
* LeoProject/LeoFS was sponsored by [Rakuten, Inc.](https://global.rakuten.com/corp/) from 2012 to Dec of 2018.
