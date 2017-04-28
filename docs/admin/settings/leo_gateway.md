# LeoGateway Settings

## Prior Knowledge

LeoGateway is a multi-protocols storage proxy, which supports REST-API over HTTP, Amazon S3-API[^1] and NFS v3[^2]. LeoGateway provides the object cache feature to handle requests efficiently and to keep the high performance of your storage system.


### Other Configurations

If you want to customize settings like where to place `leo_gateway.conf`, what user is starting a LeoGateway process and so on, refer [For Administrators / Settings / Environment Configuration](/admin/settings/environment_config.md) for more information.

## Configuration

### LeoGateway Configurations

| Item                                   | Description                              |
|----------------------------------------|------------------------------------------|
| **LeoManager Nodes**                   |
| managers                               | Name of LeoManager nodes. This configuration is necessary for communicating with `LeoManager's master` and `LeoManager's slave`.<p>( Default: [manager\_0@127.0.0.1, manager\_1@127.0.0.1] )</p> |
| **LeoGateway Basic**                   |
| protocol                               | Gateway Protocol - [s3/rest/embed/nfs] <p>( Default: s3 )</p> |
| **HTTP Related (S3/REST)**             |
| http.port                              | Port number the Gateway uses for HTTP connections <p>( Default: 8080 )</p> |
| http.num_of_acceptors                  | Numbers of processes listening for connections <p>( Default: 128 )</p> |
| http.max_keepalive                     | Maximum number of requests allowed in a single keep-alive session <p>( Default: 4096 )</p> |
| http.layer_of_dirs                     | Maximum number of virtual directory levels <p>( Default: 12 )</p> |
| http.ssl_port                          | Port number the Gateway uses for HTTPS connections <p>( Default: 8443 )</p> |
| http.ssl_certfile                      | SSL Certificate file <p>( Default: ./etc/server_cert.pem )</p> |
| http.ssl_keyfile                       | SSL key file <p>( Default: ./etc/server_key.pem )</p> |
| http.headers_config_file               | HTTP custom header configuration file <p>( Default: ./etc/http_custom_header.conf )</p> |
| http.timeout_for_header                | HTTP timeout for reading header<p>( Default: 5000, Unit: `msec`)</p> | 
| http.timeout_for_body                  | HTTP timeout for reading body<p>( Default: 15000, Unit: `msec`)</p> | 
| **Bucket Related**                     |
| bucket_prop_sync_interval              | Synchronization Interval of Bucket Properties<p>( Default: 300, Unit: `sec` )</p> |
| **NFS-related configurations**         |
| nfs.mountd.port                        | Mountd’s port number <p>( Default: 22050 )</p> |
| nfs.mountd.acceptors                   | Mountd’s the number of acceptors <p>( Default: 128 )</p> |
| nfs.nfsd.port                          | NFSd’s port number <p>( Default: 2049 )</p> |
| nfs.nfsd.acceptors                     | NFSd’s the number of acceptors <p>( Default: 128 )</p> |
| **Large object processing configuration** |
| large_object.max_chunked_objs          | Maximum number of chunked objects <p>( Default: 1000 )</p> |
| large_object.chunked_obj_len           | Length of a chunked object. This value must be >= `large_object.reading_chunked_obj_len` <p>( Default: 5242880, Unit: `byte` )</p> |
| large_object.threshold_of_chunk_len    | Threshold when object is chunked <p>( Default: 5767168, Unit: `byte` )</p> |
| large_object.reading_chunked_obj_len   | Read length of a chunked object. This value must be <= `large_object.chunked_obj_len` <p>( Default: 5242880, Unit: `byte` )</p> | 
| **Cache configuration**                |
| cache.http_cache                       | Enable HTTP-Cache mode, working like Varnish/Squid. Otherwise as Object Cache<p>( Default: false )</p>
| cache.cache_workers                    | Number of cache workers <p>( Default: 16 )</p> |
| cache.cache_ram_capacity               | Memory Cache Capacity, divide across workers. This has to satisfy `(8 * 1024 * 1024) * cache.cache_workers >= cache.cache_ram_capacity` <p>( Default: 268435456, Unit: `byte` )</p> |
| cache.cache_disc_capacity              | Disk Cache Capacity, divide across workers. This has to satisfy `(8 * 1024 * 1024) * cache.cache_workers >= cache.cache_disc_capacity` <p>( Default: 524288000, Unit: `byte` )</p> |
| cache.cache_disc_threshold_len         | Threshold when object is stored in disk cache <p>( Default: 1048576, Unit: `byte` )</p> |
| cache.cache_disc_dir_data              | Directory for disk cache data <p>( Default: ./cache/data )</p> |
| cache.cache_disc_dir_journal           | Directory for disk cache journal <p>( Default: ./cache/journal )</p> |
| **HTTP-Cache related**                 |
| cache.cache_expire                     | Cache expiry time <p>( Default: 300, Unit: `sec`)</p> |
| cache.cache_max_content_len            | Maximum length of cached object <p>( Default: 1048576, Unit: `byte` ) |
| cache.cachable_content_type            | Object types to be cached |
| cache.cachable_path_pattern            | Path pattern(s) to be cached (regular expression) |
| **Watchdog / REX**                     |
| watchdog.rex.is_enabled                | Enables or disables the rex-watchdog which monitors the memory usage of *Erlangs RPC component*.<p>( Default: true )</p> |
| watchdog.rex.interval                  | An interval of executing the watchdog processing<p>( Default: 10, Unit: `sec` )</p>  |
| **Watchdog / CPU**                     |
| watchdog.cpu.is_enabled                | Enables or disables the CPU-watchdog which monitors both *CPU load average* and *CPU utilization*<p>( Default: false )</p> |
| watchdog.cpu.raised_error_times        | Times of raising error to a client<p>( Default: 5 )</p> |
| watchdog.cpu.interval                  | An interval of executing the watchdog processing<p>( Default: 10, Unit: `sec` )</p> |
| watchdog.cpu.threshold_cpu_load_avg    | Threshold of CPU load average<p>( Default: 5.0 )</p> |
| watchdog.cpu.threshold_cpu_util        | Threshold of CPU utilization<p>( Default: 100 )</p> |
| **Watchdog / IO (Erlang VM Internal Traffic)** |
| watchdog.io.is_enabled                 | Enables or disables the IO-watchdog which monitors the *Erlang VM Internal Traffic* <p>( Default: false )</p> |
| watchdog.io.interval                   | Watchdog interval <p>( Default: 1, Unit: `sec` )</p> |
| watchdog.io.threshold_input_per_sec    | Threshold input per second <p>( Default: 134217728, Unit: `byte` )</p> |
| watchdog.io.threshold_output_per_sec   | Threshold output per second <p>( Default: 134217728, Unit: `byte` )</p> |
| **Timeout**                            |
| timeout.level_1                        | Timeout when put object to LeoStorage *(~65536 bytes)* <p> ( Default: 5000, Unit: `msec` ) |
| timeout.level_2                        | Timeout when put object to LeoStorage *(~131071 bytes)* <p> ( Default: 7000, Unit: `msec` ) |
| timeout.level_3                        | Timeout when put object to LeoStorage *(~524287 bytes)* <p> ( Default: 10000, Unit: `msec` ) |
| timeout.level_4                        | Timeout when put object to LeoStorage *(~1048576 bytes)* <p> ( Default: 20000, Unit: `msec` ) |
| timeout.level_5                        | Timeout when put object to LeoStorage *(1048576~ bytes)* <p> ( Default: 30000, Unit: `msec` ) |
| timeout.get                            | Timeout when get object from LeoStorage<p> ( Default: 30000, Unit: `msec` ) |
| timeout.ls                             | Timeout when list object from LeoStorage<p> ( Default: 30000, Unit: `msec` ) |
| **Log** |
| log.log_level                          | Log level:<ul><li>0:debug</li><li>1:info</li><li>2:warn</li><li>3:error</li></ul><p>( Default: 1 )</p> |
| log.is_enable_access_log               | Enables or disables the access-log feature<p>( Default: false )</p> |
| log.erlang                             | Destination of log file(s) of Erlang's log<p>( Default: ./log/erlang )</p> |
| log.app                                | Destination of log file(s) of LeoStorage<p>( Default: ./log/app )</p> |
| log.member_dir                         | Destination of log file(s) of members of storage-cluster<p>( Default: ./log/ring )</p> |
| log.ring_dir                           | Destination of log file(s) of RING<p>( Default: ./log/ring )</p> |
| **Other Directories Settings**         |
| queue_dir                              | Directory of queue for monitoring "RING"<p>( Default: ./work/queue )</p> |
| snmp_agent                             | Directory of SNMP agent configuration<p>( Default: ./snmp/snmpa_gateway_0/LEO-GATEWAY )</p> |

### Erlang VM's Related Configurations

| Item                             | Description                             |
|----------------------------------|-----------------------------------------|
| nodename                         | The format of the node name is `<NAME>@<IP-ADDRESS>`, which must be unique always in a LeoFS system<p>( Default: storage_0@127.0.0.1 )</p> |
| distributed_cookie               | Sets the magic cookie of the node to `Cookie`. <br/><br/>- See also: <a href="http://erlang.org/doc/reference_manual/distributed.html" target="_blank">Distributed Erlang</a><p>( Default: 401321b4 )</p> |
| erlang.kernel_poll               | Kernel poll reduces LeoFS' CPU usage when it has hundreds (or more) network connections.<p>( Default: true )</p> |
| erlang.asyc_threads              | The total number of Erlang aynch threads<p>( Default: 32 )</p> |
| erlang.max_ports                 | The max_ports sets the default value of maximum number of ports.<br/><br/>- See also: [Erlang erlang:open_port/2](http://erlang.org/doc/man/erlang.html)<p>( Default: 64000 )</p> |
| erlang.crash_dump                | The output destination of an Erlang crash dump<p>( Default: ./log/erl_crash.dump )</p> |
| erlang.max_ets_tables            | The maxinum number of <a href="http://erlang.org/doc/man/ets.html" target="_blank">Erlagn ETS</a> tables<p>( Default: 256000 )</p> |
| erlang.smp                       | `-smp` enable and `-smp` start the Erlang runtime system with <a href="https://en.wikipedia.org/wiki/Symmetric_multiprocessing" target="_blank">SMP</a> support enabled.<p>( Default: enable )</p>|
| erlang.schedulers.compaction\_of\_load   | Enables or disables scheduler compaction of load. If it's enabled, the Erlang VM will attempt to fully load as many scheduler threads as mush as possible.<p>( Default: true )</p> |
| erlang.schedulers.utilization\_balancing | Enables or disables scheduler utilization balancing of load. By default scheduler utilization balancing is disabled and instead scheduler compaction of load is enabled, which strives for a load distribution that causes as many scheduler threads as possible to be fully loaded (that is, not run out of work).<p>( Default: false )</p> |
| erlang.distribution\_buffer\_size        | Sender-side network distribution buffer size *(unit: KB)*<p>( Default: 32768 )</p> |
| erlang.fullsweep\_after                  | Option fullsweep_after makes it possible to specify the maximum number of generational collections before forcing a fullsweep, even if there is room on the old heap. Setting the number to zero disables the general collection algorithm, that is, all live data is copied at every garbage collection.<p>( Default: 0 )</p> |
| erlang.secio                             | Enables or disables eager check I/O scheduling. The flag effects when schedulers will check for I/O operations possible to execute, and when such I/O operations will execute.<p>( Default: true )</p> |
| process_limit                            | The maxinum number of Erlang processes. Sets the maximum number of simultaneously existing processes for this system if a Number is passed as value. Valid range for Number is [1024-134217727]<p>( Default: 1048576 )</p> |

### Notes and Tips of the Configuration

#### Cache Consistency between LeoGateway and LeoStorage

LeoGateway's cache feature does not depend on the consistency level of a cluster. There is a possibility of object inconsistency.

LeoGateway requests a storage node to compare a cached object's hash value with its stored object's hash value. LeoGateway selects a LeoStorage's node from RING, *a distributed hash table* by a target object name, then LeoGateway requests a LeoStorage node of the redundant node. If the requested object is inconsistent in the replicas and LeoGateway cached it, a client may get inconsistent objects.

If you need strong consistency on a LeoFS system, you can disable the cache setting.

```ini
cache.cache_ram_capacity = 0
cache.cache_disc_capacity = 0
```


#### Configuration related to Disk Cache

A total number of directories to store cache files is equal to `cache.cache_workers`. A maximum size of a cacheable object per a directory has been determined by `cache.cache_disc_capacity / cache.cache_workers`. If the size of a requested object more than the maximum size, LeoGateway avoids storing the object into the disk cache.

And also, when size of a requested object more than `cache.cache_max_content_len`, LeoGateway similarly refuses to store the object into the disk cache.

![Figure: Disk Cache Limits](../../assets/leofs-gateway-disk-cache-size.png)


## Related Links

- [Concept and Architecture / LeoGateway's Architecture](../../architecture/leo_gateway.md)
- [For Administrators / Interface / S3-API](../protocols/s3.md)
- [For Administrators / Interface / REST-API](../protocols/rest.md)
- [For Administrators / Interface / NFS v3](../protocols/nfs_v3.md)
- [For Administrators / System Operations / S3-API related Operations](../system_operations/s3.md)
- [For Administrators / Settings / Environment Configuration](/admin/settings/environment_config.md)

[^1]: <a href="http://docs.aws.amazon.com/AmazonS3/latest/API/Welcome.html" target="_blank">Amazon S3 API</a>
[^2]: <a href="https://de.wikipedia.org/wiki/Network_File_System" target="_blank">Wikipedia: Network File System</a>
