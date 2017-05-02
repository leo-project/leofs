# LeoManager Settings

## Prior Knowledge

The current version, v1.3 of LeoManager depends on <a href="http://erlang.org/doc/man/mnesia.html" target="_blank">Erlang Mnesia, A distributed telecommunications DBMS</a> to manage configurations of a LeoFS system and information of all nodes. LeoManager nodes must keep running to replicate the data for preventing data loss. You need to configure both LeoManager master and the slave.


### Other Configurations

If you want to modify settings like where to place `leo_manager.conf`, what user is starting a LeoManager process and so on, refer [For Administrators / Settings / Environment Configuration](/admin/settings/environment_config.md) for more information.


## Configuration

There are some configuration differences between LeoManager-master and LeoManager-slave. LeoManager-master only has `the consistency level` and `the multi datacenter replication`.

The default setting is to launch a LeoFS system on one node, whose setting cannot replicate data because the total number of the replica is one, and data loss could happen with high probability. You need to modify the configuration suitably before launching the LeoFS system on your production or other environments.


### LeoManager Configurations

| Item                             |  Description                             |
|----------------------------------|------------------------------------------|
| **Basic** |
| manager.partner                  | The partner of manager's alias. This configuration is necessary for communicationg between `LeoManager's master` and `LeoManager's slave`. <br/>( Default: manager_1@127.0.0.1 )|
| console.port.cui                 | The port number of LeoManager's console for text format<br/>( Default: 10010 ) |
| console.port.json                | The port number of LeoManager's console for JSON format<br/>( Default: 10020 ) |
| console.acceptors.cui            | The maximum number of acceptors of LeoManager's console for text format<br/>( Default: 3 ) |
| console.acceptors.json           | The maximum number of acceptors of LeoManager's console for JSON format<br/>( Default:16 ) |
| **System** |
| system.dc_id      | `Datacenter ID` is necessary for using the data center replication<br/>( Default: dc_1 ) |
| system.cluster_id | `Cluster ID` is also necessary for using the data center replication<br/>( Default: leofs_1 ) |
| **[Consistency Level](cluster.md)** |
| consistency.num\_of\_replicas     | `only LeoManager's master`<br/> The total number of object copies<br/>( Default: 1 ) |
| consistency.write                 | `only LeoManager's master`<br/> The total number of object copies needed for a successful WRITE operation<br/>( Default: 1 ) |
| consistency.read                  | `only LeoManager's master`<br/> The total number of object copies needed for a successful READ operation<br/>( Default: 1 ) |
| consistency.delete                | `only LeoManager's master`<br/> The total number of object copies needed for a successful DELETE operation<br/>( Default: 1 ) |
| consistency.rack\_aware\_replicas | `only LeoManager's master`<br/> The total number of object copies of rack-aware<br/>( Default: 0 ) |
| **Multi Data Center Replication** |
| mdc_replication.max\_targets              | `only LeoManager's master`<br/> The maximum number of replication targets of clusters OR data centers<br/>( Default: 2 ) |
| mdc\_replication.num\_of\_replicas\_a\_dc | `only LeoManager's master`<br/> A remote cluster of a LeoFS system which receives this cluster's objects, and then replicates them, which adhere to a replication method of each object<br/>( Default: 1 )|
| mdc\_replication.consistency.write  | `only LeoManager's master` `[since 1.3.3]` <br/> A number of replicas needed for a successful WRITE-operation<br/>( Default: 1 ) |
| mdc\_replication.consistency.read   | `only LeoManager's master` `[since 1.3.3]` <br/> A number of replicas needed for a successful READ-operation<br/>( Default: 1 ) |
| mdc\_replication.consistency.delete | `only LeoManager's master` `[since 1.3.3]` <br/> A number of replicas needed for a successful DELETE-operation<br/>( Default: 1 ) |
| **RPC for Multi Datacenter Replication** |
| rpc.server.acceptors                | The total number of acceptor of the RPC server<br/>( Default: 16 ) |
| rpc.server.listen\_port             | The listening port of the RPC server<br/>( Default: 13075 ) |
| rpc.server.listen\_timeout          | The listening timeout<br/>( Default: 5000 ) |
| rpc.client.connection\_pool\_size   | A client is able to keep connections of a remote LeoFS up to the pool size<br/>( Default: 16 ) |
| rpc.client.connection\_buffer\_size | A client is able to increase connections of a remote LeoFS up to the buffer size<br/>( Default: 16 ) |
| **Mnesia** |
| mnesia.dir                         | The directory of the database file of Mnesia*(Erlang distributed DB)*<br/>( Default: ./work/mnesia/127.0.0.1 ) |
| mnesia.dump\_log\_write\_threshold | The maximum number of writes allowed to the transaction log before a new dump of the log is performed. Default is 100 log writes.<br/><br/>- See also: <a href="http://erlang.org/doc/man/mnesia.html#dump_log_write_threshold" target="_blank">Erlang Mnesia dump_log_write_threshold</a><br/>( Default: 50000 ) |
| mnesia.dc\_dump\_limit             | Mnesia's tables are dumped when *filesize(Log) > (filesize(Tab)/Dc_dump_limit)*. Lower values reduce CPU overhead but increase disk space and startup times. Default is 4.<br/><br/>- See also: <a href="http://erlang.org/doc/man/mnesia.html" target="_blank">Erlang Mnesia</a><br/>( Default: 40 ) |
| **Log** |
| log.log_level       | LeoManager's logger controls outputting logs by the log level:<ul><li>1: Info</li><li>2: Warn</li><li>3: Error</li></ul>( Default: 1 ) |
| log.erlang          | The output destination of Erlang's logs<br/>( Default: ./log/erlang ) |
| log.app             | The output destination of LeoManager's logs<br/>( Default: ./log/app ) |
| log.member_dir      | The output destination of the member's dump file<br/>( Default: ./log/ring ) |
| log.ring_dir        | The output destination of the RING's dump file<br/>( Default: ./log/ring ) |
| **Other Directories** |
| queue_dir  | The directory of the data file of LeoFS' MQ<br/>( Default: ./work/queue ) |
| snmp_agent | The directory of the snmp agent file of LeoFS<br/>( Default: ./snmp/snmpa_manager_0/LEO-MANAGER ) |


### Erlang VM's Related Configurations

| Item                             | Description                              |
|----------------------------------|------------------------------------------|
| nodename                         | The format of the node name is `<NAME>@<IP-ADDRESS>`, which must be unique always in a LeoFS system<br/>( Default: manager_0@127.0.0.1 ) |
| distributed\_cookie              | Sets the magic cookie of the node to `Cookie`. <br/><br/>- See also: <a href="http://erlang.org/doc/reference_manual/distributed.html" target="_blank">Distributed Erlang</a><br/>( Default: 401321b4 ) |
| erlang.kernel\_poll              | Kernel poll reduces LeoFS' CPU usage when it has hundreds (or more) network connections<br/>( Default: true ) |
| erlang.asyc\_threads             | The total number of Erlang aynch threads for the async thread pool. *The asynchronous thread pool* are OS threads which are userd for I/O operations.<br/>( Default: 32 ) |
| erlang.max\_ports                | The max_ports sets the default value of maximum number of ports.<br/><br/>- See also: [Erlang erlang:open_port/2](http://erlang.org/doc/man/erlang.html)<br/>( Default: 64000 ) |
| erlang.crash\_dump               | The output destination of an Erlang crash dump<br/>( Default: ./log/erl_crash.dump ) |
| erlang.max\_ets\_tables          | The maxinum number of <a href="http://erlang.org/doc/man/ets.html" target="_blank">Erlagn ETS</a> tables<br/>( Default: 256000 ) |
| erlang.smp                       | `-smp` enable and `-smp` start the Erlang runtime system with <a href="https://en.wikipedia.org/wiki/Symmetric_multiprocessing" target="_blank">SMP</a> support enabled<br/>( Default: enable ) |
| process\_limit                   | The maxinum number of Erlang processes. Sets the maximum number of simultaneously existing processes for this system if a Number is passed as value. Valid range for Number is [1024-134217727]<br/>( Default: 1048576 )|


## Related Links

* [Concept and Architecture / LeoManager's Architecture](/architecture/leo_manager.md)
* [For Administrators / Settings / Cluster Settings](cluster.md)
* [For Administrators / Settings / Environment Configuration](/admin/settings/environment_config.md)
* [For Administrators / System Operations / Multi Data Center Replication](/admin/system_operations/multi_dc_replication.md)
