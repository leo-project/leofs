# Multi Data Center Replication
## Configuration

LeoFS provides the multi data center replication related configuration items, which contain `leo_manager_0.conf`. Modify those configuration items to work its feature correctly.

### LeoManager Master's Configuration For MDCR

| Item | Description |
|------|-------------|
| `mdc_replication.num_of_replicas_a_dc` | A remote cluster of a LeoFS system which receives this cluster's objects, and then replicates them, which adhere to a replication method of each object |
| `mdc_replication.consistency.write` | A number of replicas needed for a successful WRITE-operation |
| `mdc_replication.consistency.read` | A number of replicas needed for a successful READ-operation |
| `mdc_replication.consistency.delete` | A number of replicas needed for a successful DELETE-operation |


## How To Operate Multi Data Center Replication
### Commands

There are three commands, `join-cluster`, `remove-cluster`, and `cluster-status`. You can control the multi data center repliation by LeoFS' CUI, `leofs-adm`.

| Command | Description |
|---------|-------------|
| `join-cluster <RMM> <RMS>`   | Begin to communicate between the local cluster and the remote cluster |
| `remove-cluster <RMM> <RMS>` | Terminate to communicate between the local cluster and the remote cluster |
| `cluster-status` | See the current state of cluster(s) |

- `RMM`: A Remote cluster's LeoMnager-Master
- `RMS`: A Remote cluster's LeoManager-Slave

#### join-cluster

```text
$ leofs-adm join-cluster manager_10@127.0.0.1:13095 manager_11@127.0.0.1:13096
OK
```

#### remove-cluster

```text
$ leofs-adm remove-cluster manager_10@127.0.0.1:13095 manager_11@127.0.0.1:13096
OK
```

#### cluster-status

```text
$ leofs-adm cluster-status
cluster id |   dc id    |    status    | # of storages  |          updated at
-----------+------------+--------------+----------------+-----------------------------
leofs_2    | dc_2       | running      |              3 | 2017-04-26 10:23:59 +0900
```


## Related Links

- [For Administrators / Settings / LeoManager Settings](/admin/settings/leo_manager.md)