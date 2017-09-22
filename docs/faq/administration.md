# FAQ: LeoFS Administration

<!-- toc -->

## Where can I get the packages for LeoFS?

You can get the packages by platform from <a href="http://leo-project.net/leofs/download.html" target="_blank">LeoFS website</a>, and LeoFS package for FreeBSD has been published at <a href="http://www.freshports.org/databases/leofs" target="_blank">FreeBSD Fresh Ports</a>.

## How can I run my LeoFS cluster automatically?

Currently after restarting a storage node,  you need to issue the resume command with leofs-adm script manually like this.

```bash
$ leofs-adm resume storage_0@127.0.0.1
```

We're planing to provide LeoFS's automated operations like `auto-compaction`, `auto-rebalance` and others with LeoFS v1.2, v1.4 and v2.0. Actually, `auto-compaction` which was already supported with v1.2.


## How do multiple users login into LeoFS Manager's console at the same time?

### Answer 1:

Actually, there is no login status in LeoFS, but the number of listening tcp connections is limited by [leo_manger.conf](https://github.com/leo-project/leofs/blob/master/apps/leo_manager/priv/leo_manager_0.conf) `default: console.acceptors.cui = 3`.
So while using default settings, 3 connections can be connected to a manager in parallel.

### Answer 2:

Since `LeoFS v1.1.0`, LeoFS have the more powerful alternative option [leofs-adm](https://github.com/leo-project/leofs/blob/master/leofs-adm) command.

This command have not only same functionalities with the existing telnet way but also do NOT keep an tcp connection established for long time (connect only when issueing a command). You don't need to worry about the number of tcp connections.

## The result of the du command can be different with the actual disk-usage

In order to reduce system resource usage for calculating the result of the `du` command, LeoFS keep that information on memory and when stopping itself, LeoFS save those data into a local file. Then when restarting, LeoFS load those on memory.

So if LeoFS is stopped unintentionally like killing by OOM killer, those data can become inconsistent with actual usage.

If you get into this situation, you can recover those data by issueing the `compact-start` command to the node. after finishing the compaction, the result of the du command will be consitent with actual usage.


## When issueing the recover node command the LeoFS can get into high load

Since the `recover-node` command can lead to issue lots of disk I/O and consume network bandwidth, if you face to issue the recover-node to multiple nodes at once, LeoFS can get into high load and become unresponsive. So we recommend you execute the recover-node command to target nodes one by one.

If this solution could not work for you, you're able to control how much recover-node consume system resources by changing [the MQ-related parameters in leo_storage.conf](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_storage/priv/leo_storage.conf#L214-L230).


## What should I do when Too many processes errors happen?

LeoFS usually try to keep the number of Erlang processes as minimum as possible, but there are some exceptions when doing something asynchronously.

* Replicating an object to the non-primary assigned nodes
* Retrying to replicate an object when the previous attempt failed

Given that LeoFS suffered from very high load AND there are some nodes downed for some reason, The number of Erlang processes gradually have increased and might have reached the sysmte limit.

We recommend users to set an appropriate value which depends on your workload to the [+P option](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_storage/priv/leo_storage.conf#L438). Also if the [+P option](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_storage/priv/leo_storage.conf#L438) does NOT work for you, there are some possibilities that some external system resources like disk, network equipments have broken, Please check out the dmesg/syslog on your sysmtem.


## How do I set "a number of containers" at LeoFS Storage configuration?

Objects/files are stored into LeoFS Storage containers which are log-structured files. LeoFS has the `data-compaction` mechanism in order to remove unncessary objects/files from the object-containers.

LeoFS's performance is affected by the data-compaction. And also, LeoFS Storage temporally creates a new file of a object-container corresponding to the compaction target container, which means during the data-compaction needs disk space for the new file of object-container(s).

If `write, update and delete operation` is a lot, we recommend that the number of containers is 32 OR 64 because it's possible to make effect of the data-compaction at a minimum as much as possible.

### In conclusiton:

* Read operation > Write operation:
    * [A total number of containers](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_storage/priv/leo_storage.conf#L46) = 8
* A lot of write, update and delete operation:
    * [A total number of containers](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_storage/priv/leo_storage.conf#L46) = 32 OR 64 *(depends on the disk capacity and performance)*


## leo_storage can not start due to "enif_send_failed on non smp vm"

When starting `leo_storage` on a single core machine which crashes with an erl_nif error.

```bash
## Error log
enif_send: env==NULL on non-SMP VM
Aborted (core dumped)
```

In this case, you have faced with [LeoFS Issue#320](https://github.com/leo-project/leofs/issues/302). You need to set a Erlang's VM flag, [-smp](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_storage/priv/leo_storage.conf#L417) in your `leo_storage.conf` as follows:

```bash
## leo_storage.conf
erlang.smp = enable
```

## Why is the speed of rebalance/recover command too slow?

You must hit [LeoFS Issue#359](https://github.com/leo-project/leofs/issues/359). Since this issue has been fixed with `LeoFS v1.2.9`, we'd recommend you upgrading to the v1.2.9 or higher one, and set an appropriate value for your environment to [mq.num_of_mq_procs](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_storage/priv/leo_storage.conf#L212) in your leo_storage.conf.


## Why does LeoFS's SNMP I/F give me wrong values(0) instead of correct values?

You must hit [LeoFS Issue#361](https://github.com/leo-project/leofs/issues/361). Since this issue has been fixed with `LeoFS v1.2.9`, we'd recommend you upgrading to the v1.2.9 or higher one.

## When adding a new storage node, that node doesn't appear with `leofs-adm status`, Why?

If you changed a WRONG node name before stopping the daemon, As a result, when a new daemon was starting, it failed to detect that the previous was still running and
stop command did not work too.

Since you can notice this kind of mistake in `error.log` with `LeoFS v1.2.9`, we'd recommend you upgrading to the v1.2.9 or higher one.

## What should I do when a **timeout** error happen during upload a very large file?

There are two configurations that could affect how often **timeout** could happen in leo_gateway.conf.

- http.timeout_for_body (in msecs)
- large_object.reading_chunked_obj_len (in bytes)

As **timeout** during upload could happen when `http.timeout_for_body` passed during every `large_object.reading_chunked_obj_len` bytes read, assuming we have `http.timeout_for_body` set to 1000 and `large_object.reading_chunked_obj_len` set to 1048576, we could expect any upload to be succeeded in if we ensured at least 8Mbps(1MB/sec) stable network bandwidth.

Also taking the concurrent uploads into account, we can define the expression that enables us to check whether each configuration is valid as below.

```ini
EXPECTED_BANDWIDTH_IN_BYTES = large_object.reading_chunked_obj_len / (http.timeout_for_body / 1000) * MAX_CONCURRENT_UPLOADS
```

For example, say we have 1Gbps(128MB/sec) stable bandwidth and 128 concurrent uploads at most then we could be ready to 1MB per connection so the below is considered as a valid configuration.

```ini
http.timeout_for_body = 1000
large_object.reading_chunked_obj_len = 1048576
# EXPECTED_BANDWIDTH_IN_BYTES = 1048576 / (1000 / 1000) * 128 = 134217728(128MB)
```

Also there is one important thing you have to care.
To decrease the bandwidth and odds timeout could happen, you have two options.

- Decrease `large_object.reading_chunked_obj_len`
- Increase `http.timeout_for_body`

Logically it's same between decreasing the buffer size and increasing the timeout. However there is one benefit using the former one rather than the latter. As the memory allocation request in Erlang VM happen in `large_object.reading_chunked_obj_len` unit, the larger buffer size we set, the more memory footprint the host running LeoGateway needs at once so that if LeoGateway accepts lots of connections and those try to upload very large files in parallel then the odds OOM could happen increase. That said, decreasing the buffer size should be the first try in terms of safety(less memory usages).

## If nodes in a cluster are connected using DNS names, When will DNS lookups happen?
* The short answer is
    * ONLY when starting each node
    * Rarely when the network gets unstable, and TCP connections between nodes get disconnected and re-connected later (TCP connections between nodes in a cluster keep alive and being used for RPC(remote procedure call) and any Distributed Erlang[^1] feature, so once connected to others, No DNS lookups happen except abnormal cases as stated above)
* The long/precious answer is depending on how Distributed Erlang[^1] is implemented so you would have to become a master of Distributed Erlang[^1] protocol, however in reality only remembering the short answer could help you in almost cases

Given that the above answers, You would not need to be afraid of using hostnames instead of ip addresses.

## Why doesn't leofs-adm respond to a command like leofs-adm status?
The netcat installed on your env is probably the one distributed through netcat-traditional package and unfortunately that doesn't work for leofs-adm which requires netcat-openbsd instead. if that is the case, installing netcat-openbsd should solve the problem as reported on github issue[^2].

[^1]: <a href="http://erlang.org/doc/reference_manual/distributed.html" target="_blank">Distributed Erlang</a>
[^2]: <a href="https://github.com/leo-project/leofs/issues/519" target="_blank">LEOFS' ISSUE_519</a>
