# LeoManager Settings

LeoManager is to monitor state of LeoGateway's nodes and LeoStorage's nodes to keep high availability of your LeoFS, and to deliver [LeoFS' operation commands](../index_of_commands.md). RING - *distributed hash table* consistency of every node is always monitored by LeoManager to avoid <a href="https://en.wikipedia.org/wiki/Split-brain" target="_blank">split-brain</a>.


## Configuration
### LeoManager's Related Configurations

| Item                             | Default              | Description                             |
|----------------------------------|----------------------|-----------------------------------------|
| **Basic** |
| manager.partner                  | manager_1@127.0.0.1  | The partner of manager's alias. This configuration is necessary for communicationg between `LeoManager's master` and `LeoManager's slave`. |
| console.port.cui                 | 10010                | The port number of LeoManager's console for text format |
| console.port.json                | 10020                | The port number of LeoManager's console for JSON format |
| console.acceptors.cui            | 3                    | The maximum number of acceptors of LeoManager's console for text format |
| console.acceptors.json           | 16                   | The maximum number of acceptors of LeoManager's console for JSON format |
| console.histories.num_of_display | 200                  | The maximum number of histories to display at once |
| **System** |
| system.dc_id      | dc_1    | `Datacenter ID` is necessary for using the data center replication. |
| system.cluster_id | leofs_1 | `Cluster ID` is also necessary for using the data center replication. |
| **[Consistency Level](cluster.md)** |
| consistency.num\_of\_replicas     | 1 | `only LeoManager's master`<br/> The total number of object copies |
| consistency.write                 | 1 | `only LeoManager's master`<br/> The total number of object copies needed for a successful WRITE operation |
| consistency.read                  | 1 | `only LeoManager's master`<br/> The total number of object copies needed for a successful READ operation |
| consistency.delete                | 1 | `only LeoManager's master`<br/> The total number of object copies needed for a successful DELETE operation |
| consistency.rack\_aware\_replicas | 0 | `only LeoManager's master`<br/> The total number of object copies of rack-aware |
| **Multi Data Center Replication** |
| mdc_replication.max\_targets              | 2 | The maximum number of replication targets of clusters OR data centers |
| mdc_replication. num\_of\_replicas\_a\_dc | 1 | An object is transmitted to a remote cluster or data center, and the total number of object copies per a data center is this configuration. |
| **RPC for Multi Datacenter Replication** |
| rpc.server.acceptors                | 16    | The total number of acceptor of the RPC server |
| rpc.server.listen\_port             | 13075 | The listening port of the RPC server |
| rpc.server.listen\_timeout          | 5000  | The listening timeout |
| rpc.client.connection\_pool\_size   | 16    | A client is able to keep connections of a remote LeoFS up to the pool size.  |
| rpc.client.connection\_buffer\_size | 16    | A client is able to increase connections of a remote LeoFS up to the buffer size. |
| **Mnesia** |
| mnesia.dir                         | ./work/mnesia/127.0.0.1 | The directory of the database file of Mnesia*(Erlang distributed DB)* |
| mnesia.dump\_log\_write\_threshold | 50000                   | The maximum number of writes allowed to the transaction log before a new dump of the log is performed. Default is 100 log writes.<br/><br/>- See also: <a href="http://erlang.org/doc/man/mnesia.html#dump_log_write_threshold" target="_blank">Erlang Mnesia dump_log_write_threshold</a> |
| mnesia.dc\_dump\_limit             | 40                      | Mnesia's tables are dumped when *filesize(Log) > (filesize(Tab)/Dc_dump_limit)*. Lower values reduce CPU overhead but increase disk space and startup times. Default is 4.<br/><br/>- See also: <a href="http://erlang.org/doc/man/mnesia.html" target="_blank">Erlang Mnesia</a>|
| **Log** |
| log.log_level       | 1            | LeoManager's logger controls outputting logs by the log level. <br/><br/>[Log level]<br/> 1: Info<br/> 2: Warn<br/> 3: Error |
| log.erlang          | ./log/erlang | The output destination of Erlang's logs |
| log.app             | ./log/app    | The output destination of LeoManager's logs |
| log.member_dir      | ./log/ring   | The output destination of the member's dump file |
| log.ring_dir        | ./log/ring   | The output destination of the RING's dump file |
| **Other Directories** |
| queue_dir  |./work/queue | The directory of the data file of LeoFS' MQ |
| snmp_agent | ./snmp/snmpa_manager_0/LEO-MANAGER| The directory of the snmp agent file of LeoFS |


### Erlang VM's Related Configurations

| Item                             | Default              | Description                             |
|----------------------------------|----------------------|-----------------------------------------|
| nodename                         | manager_0@127.0.0.1  | You need to set the nodename which is as follows: `<ALIAS>@<IP-ADDRESS>` |
| distributed_cookie               | 401321b4             | Sets the magic cookie of the node to `Cookie`. <br/><br/>- See also: <a href="http://erlang.org/doc/reference_manual/distributed.html" target="_blank">Distributed Erlang</a> |
| erlang.kernel_poll               | true                 | Kernel poll reduces LeoFS' CPU usage when it has hundreds (or more) network connections. |
| erlang.asyc_threads              | 32                   | The total number of Erlang aynch threads |
| erlang.max_ports                 | 64000                | The max_ports sets the default value of maximum number of ports.<br/><br/>- See also: [Erlang erlang:open_port/2](http://erlang.org/doc/man/erlang.html) |
| erlang.crash_dump                | ./log/erl_crash.dump | The output destination of an Erlang crash dump |
| erlang.max_ets_tables            | 256000               | The maxinum number of <a href="http://erlang.org/doc/man/ets.html" target="_blank">Erlagn ETS</a> tables |
| erlang.smp                       | enable               | `-smp` enable and `-smp` start the Erlang runtime system with <a href="https://en.wikipedia.org/wiki/Symmetric_multiprocessing" target="_blank">SMP</a> support enabled.|
| process_limit                    | 1048576              | The maxinum number of Erlang processes. Sets the maximum number of simultaneously existing processes for this system if a Number is passed as value. Valid range for Number is [1024-134217727]|
| snmp_conf                        | ./snmp/snmpa_manager_0/leo_manager_snmp | The file path of the snmp agent |



## Related Links

* [Settings / Cluster Settings](cluster.md)
