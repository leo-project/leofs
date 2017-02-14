# Index of leofs-adm Command Lines

[leofs-adm](https://github.com/leo-project/leofs/blob/master/leofs-adm) easily makes administrative operations of LeoFS, the commands of which include as below:

* General Commands
* LeoStorage Cluster Operation
* LeoStorage MQ Operation
* Recover Commands
* Data Compaction Commands
* Disk Usage Commands
* LeoGateway Operation
* LeoManager Maintenance Commands
* S3-API Related Commands
* Multi Data Center Operation


| Command                               | Description |
|---------------------------------------|-------------|
| **General Commands:**                 | |
| status `[<node>]`                     | Retrieve status of every node (default)<br/> Retrieve status of a specified node |
| whereis `<file-path>`                 | Retrieve an assigned object by a file path |
| **Storage Operation:**                | |
| detach `<storage-node>`               | Remove a storage node in a LeoFS' storage cluster<br/>Current status: `running` OR `stop` |
| suspend `<storage-node>`              | Suspend a storage node for maintenance<br/>This command does NOT detach a node from a LeoFS' storage cluster<br/>While suspending, it rejects any requests<br/>Current status: `running`|
| resume `<storage-node>`               | Resume a storage node for finished maintenance<br/>Current status: `suspended` OR `restarted`|
| start                                 | Start LeoFS after distributing a RING from LeoFS Manager to LeoFS Storage and LeoFS Gateway|
| rebalance                             | Commit detached and attached nodes to join a cluster<br/>Rebalance objects in a cluster based on updated cluster topology|
| mq-stats `<storage-node>`             | See statuses of message queues used in a LeoStorage node |
| mq-suspend `<storage-node>` `<mq-id>` | Suspend a process consuming a message queue<br/>Active message queues only can be suspended<br/>While suspending, no messages are consumed |
| mq-resume `<storage-node>` `<mq-id>`  | Resume a process consuming a message queue |
| **Recover Commands:**                 | |
| recover-file `<file-path>`            | Recover an inconsistent object specified by a file-path |
| recover-node `<storage-node>`         | Recover all inconsistent objects in a specified node |
| recover-ring `<storage-node>`         | Recover `RING`, a routing table of a specified node |
| recover-cluster `<cluster-id>`        | Recover all inconsistent objects in a specified cluster in case of using the multi datacenter replication |
| **Compaction Commands:**              | |
| compact-start `<node>` `<num-of-targets>` [`<number-of-compaction-procs>`] | Remove unnecessary objects from a specified node<br/> `num-of-targets`: It controls a number of containers in parallel <br/> `num-of-compaction-procs`: It controls a number of procs to execute the data compaction in parallel|
| compact-suspend `<storage-node>`      | Suspend a data compaction processing |
| compact-resume `<storage-node>`       | Resume a data compaction processing |
| compact-status `<storage-node>`       | See current compaction status<br/> Compaction’s status: `idle`, `running`, `suspend` |
| diagnose-start `<node>`               | Diagnose data of a specified storage node |
| **Disk Usage Commands:**              | |
| du `<storage-node>`        | See current disk usages|
| du detail `<storage-node>`        | See current disk usages in detail |
| **Gateway Operation:**                | |
| purge-cache `<file-path>`             | Remove a cache from each LeoFS gateway　|
| remove-gateway `<gateway-node>`       | Remove a specified LeoGateway node, which is already stopped　|
| **Manager Maintenance:**              | |
| backup-mnesia `<backup-filepath>`     | Copy LeoFS’s Manager data to a filepath　|
| restore-mnesia `<backup-filepath>`    | Restore LeoFS’s Manager data from a backup file|
| update-managers `<manager-master>` `<manager-slave>` | Update LeoFS Manager nodes<br/> Destribute a new LeoManager nodes to LeoStorage and LeoGateway|
| dump-ring `<node>` | Dump RING, a routing table to a local disk |
| update-log-level `gateway/storage-node>` `<log-level>` | Update log level of a specified node<br/> log-level: debug, info, warn, error|
| update-consistency-level <`write-quorum>` `<read-quorum>` `<delete-quorum>`  | Update current consistency level of `R-quorum`, `W-quorum` and `D-quorum` |
| **Watchdog Operation:**               | |
| update-property `<node>` `<property-name>` `<property-value>` | Update watchdog properties of a specifid node, which includes as below:<br>- watchdog.cpu_enabled `<boolean>`<br/>- watchdog.cpu_raised_error_times `<integer>`<br/>- watchdog.cpu_interval `<integer>`<br/>- watchdog.cpu_threshold_load_avg `<float>`<br/>- watchdog.cpu_threshold_util `<integer>`<br/>- watchdog.disk_enabled `<boolean>`<br/>- watchdog.disk_raised_error_times `<integer>`<br/>- watchdog.disk_interval `<integer>`<br/>- watchdog.disk_threshold_use `<integer>`<br/>- watchdog.disk_threshold_util `<integer>`<br/>- watchdog.disk_threshold_rkb `<integer>`<br/>- watchdog.disk_threshold_wkb `<integer>`<br/>- watchdog.cluster_enabled `<boolean>`<br/>- watchdog.cluster_interval `<integer>`<br/>|
| **S3-related Commands:**              | |
| create-user `<user-id>` `[<password>]` | Register a new user<br/> Generate an S3 key pair, `Access Key ID` and `Secret Access Key` |
| import-user `<user-id>` `<access-key-id>` `<secret-access-key>` | Import a new user with S3 key pair, `Access Key ID` and `Secret Access Key`|
| delete-user `<user-id>` | Remove a user |
| get-users | Retrieve a list of users |
| update-user-role `<user-id>` `<role-id>` | Update a user’s role<br/> Currently, we are supporting two kinds of roles, [1: `General user`], [9: `Administrator`] |
| add-endpoint `<endpoint>` | Register a new S3 Endpoint<br/> LeoFS’ domains are ruled by <a href="https://docs.aws.amazon.com/AmazonS3/latest/dev/VirtualHosting.html" target="_blank">Naming rule of AWS S3 Bucket</a> |
| delete-endpoint `<endpoint>` | Remove an endpoint |
| get-endpoints | Retrieve a list of endpoints |
| add-bucket `<bucket>` `<access-key-id>` | Create a new bucket |
| delete-bucket `<bucket>` `<access-key-id>` | Remove a bucket and all files stored in the bucket |
| get-bucket `<access-key-id>` | Retrieve a list of buckets owned by a specified user |
| get-buckets | Retrieve a list of all buckets registered |
| chown-bucket `<bucket>` `<access-key-id>` | Change an owner of a bucket |
| update-acl `<bucket>` `<access-key-id>` `<acl>` | Update ACL, Access Control List for a bucket<br/>- `private (default)`: No one except an owner has access rights<br/>- `public-read`: All users have READ access<br/>- `public-read-write`: All users have READ and WRITE access|
| gen-nfs-mnt-key `<bucket>` `<access-key-id>` `<client-ip-address>` | Generate a key for NFS mount |
| **Multi Data Center Operation:** | |
| join-cluster `<remote-manager-master>` `<remote-manager-slave>` | Begin to communicate between a local cluster and a remote cluster |
| remove-cluster `<remote-manager-master>` `<remote-manager-slave>` | Terminate to communicate between a local cluster and a remote cluster |
| cluster-status | See a current state of cluster(s) |
