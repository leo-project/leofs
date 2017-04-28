# Cluster Operations
## Prior Knowledge

LeoFS provides the cluster operation features which are implemented on <a href="https://github.com/leo-project/leofs/blob/master/leofs-adm" target="_blank">leofs-adm</a>, LeoFS CLI for administration. LeoFS supports `node addition` and `node deletion`, and already covers as unique features of LeoFS, `node suspension`, `node restart`, and `node takeover`. You can use those functions after starting a LeoFS system.

## Operations
### Add a Node

LeoFS temporally adds a node into the member table of LeoManager's database after launching a new LeoStorage node. If you decide to join it in the cluster, you need to execute [leofs-adm rebalance](#rebalance-a-cluster) command.

```bash
## Example:
## 1. Launch a new LeoStorage node

## 2. Check the current state of the cluster
$ leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.3.3
                        cluster Id | leofs_1
                             DC Id | dc_1
                    Total replicas | 2
          number of successes of R | 1
          number of successes of W | 1
          number of successes of D | 1
 number of rack-awareness replicas | 0
                         ring size | 2^128
-----------------------------------+----------
 Multi DC replication settings
-----------------------------------+----------
        max number of joinable DCs | 2
           number of replicas a DC | 1
-----------------------------------+----------
 Manager RING hash
-----------------------------------+----------
                 current ring-hash | d5d667a6
                previous ring-hash | d5d667a6
-----------------------------------+----------

 [State of Node(s)]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | storage_0@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:20:19 +0900
  S    | storage_1@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:20:19 +0900
  S    | storage_2@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:20:19 +0900
  S    | storage_3@127.0.0.1      | attached     |                |                | 2017-04-18 18:20:37 +0900
  G    | gateway_0@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:20:21 +0900
-------+--------------------------+--------------+----------------+----------------+----------------------------

## 3. Execute `rebalance`
$ leofs-adm rebalance
Generating rebalance-list...
Generated rebalance-list
Distributing rebalance-list to the storage nodes
OK  25% - storage_0@127.0.0.1
OK  50% - storage_1@127.0.0.1
OK  75% - storage_2@127.0.0.1
OK 100% - storage_3@127.0.0.1
OK

## 4. Check the latest state of cluster after rebalancing the cluster
$ leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.3.3
                        cluster Id | leofs_1
                             DC Id | dc_1
                    Total replicas | 2
          number of successes of R | 1
          number of successes of W | 1
          number of successes of D | 1
 number of rack-awareness replicas | 0
                         ring size | 2^128
-----------------------------------+----------
 Multi DC replication settings
-----------------------------------+----------
        max number of joinable DCs | 2
           number of replicas a DC | 1
-----------------------------------+----------
 Manager RING hash
-----------------------------------+----------
                 current ring-hash | ce4bece1
                previous ring-hash | 3923d007
-----------------------------------+----------

 [State of Node(s)]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | storage_0@127.0.0.1      | running      | ce4bece1       | 3923d007       | 2017-04-18 18:20:19 +0900
  S    | storage_1@127.0.0.1      | running      | ce4bece1       | 3923d007       | 2017-04-18 18:20:19 +0900
  S    | storage_2@127.0.0.1      | running      | ce4bece1       | 3923d007       | 2017-04-18 18:20:19 +0900
  S    | storage_3@127.0.0.1      | running      | ce4bece1       | 3923d007       | 2017-04-18 18:21:25 +0900
  G    | gateway_0@127.0.0.1      | running      | ce4bece1       | 3923d007       | 2017-04-18 18:20:21 +0900
-------+--------------------------+--------------+----------------+----------------+----------------------------
```

### Remove a Node

If you need to shrink a target LeoFS' cluster size, you can realize that by following the operation flow.

- Decide to remove a LeoStorage node, whose state must be `running` or `stop`
- Then execute `leofs-adm detach` command
- Finally, execute `leofs-adm rebalance` command to start rebalancing data in the cluster

```bash
## Example:
## 1. Check the current state of the cluster
$ leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.3.3
                        cluster Id | leofs_1
                             DC Id | dc_1
                    Total replicas | 2
          number of successes of R | 1
          number of successes of W | 1
          number of successes of D | 1
 number of rack-awareness replicas | 0
                         ring size | 2^128
-----------------------------------+----------
 Multi DC replication settings
-----------------------------------+----------
        max number of joinable DCs | 2
           number of replicas a DC | 1
-----------------------------------+----------
 Manager RING hash
-----------------------------------+----------
                 current ring-hash | 3923d007
                previous ring-hash | 3923d007
-----------------------------------+----------

 [State of Node(s)]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | storage_0@127.0.0.1      | running      | 3923d007       | 3923d007       | 2017-04-18 18:31:37 +0900
  S    | storage_1@127.0.0.1      | running      | 3923d007       | 3923d007       | 2017-04-18 18:31:37 +0900
  S    | storage_2@127.0.0.1      | running      | 3923d007       | 3923d007       | 2017-04-18 18:31:37 +0900
  S    | storage_3@127.0.0.1      | running      | 3923d007       | 3923d007       | 2017-04-18 18:31:37 +0900
  G    | gateway_0@127.0.0.1      | running      | 3923d007       | 3923d007       | 2017-04-18 18:31:55 +0900
-------+--------------------------+--------------+----------------+----------------+----------------------------

## 2. Remove a LeoStorage node
$ leofs-adm detach storage_3@127.0.0.1
OK

## 3. Execute `rebalance`
$ leofs-adm rebalance
Generating rebalance-list...
Generated rebalance-list
Distributing rebalance-list to the storage nodes
OK  33% - storage_0@127.0.0.1
OK  67% - storage_1@127.0.0.1
OK 100% - storage_2@127.0.0.1
OK

## 3. Check the latest state of cluster after rebalancing the cluster
$ leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.3.3
                        cluster Id | leofs_1
                             DC Id | dc_1
                    Total replicas | 2
          number of successes of R | 1
          number of successes of W | 1
          number of successes of D | 1
 number of rack-awareness replicas | 0
                         ring size | 2^128
-----------------------------------+----------
 Multi DC replication settings
-----------------------------------+----------
        max number of joinable DCs | 2
           number of replicas a DC | 1
-----------------------------------+----------
 Manager RING hash
-----------------------------------+----------
                 current ring-hash | d5d667a6
                previous ring-hash | d5d667a6
-----------------------------------+----------

 [State of Node(s)]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | storage_0@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:31:37 +0900
  S    | storage_1@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:31:37 +0900
  S    | storage_2@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:31:37 +0900
  G    | gateway_0@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:31:55 +0900
-------+--------------------------+--------------+----------------+----------------+----------------------------
```

### Take Over a Node

If a new LeoStorage node takes over a detached node, you can realize that by following the operation flow.

- Execute `leofs-adm detach` command to remove a target node in the cluster
- Then launch a new node to take over the detached node
- Finally, execute `leofs-adm reebalance` command to start rebalancing data in the cluster

```bash
## Example:
## 1. Check the current state of the cluster (1)
$ leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.3.3
                        cluster Id | leofs_1
                             DC Id | dc_1
                    Total replicas | 2
          number of successes of R | 1
          number of successes of W | 1
          number of successes of D | 1
 number of rack-awareness replicas | 0
                         ring size | 2^128
-----------------------------------+----------
 Multi DC replication settings
-----------------------------------+----------
        max number of joinable DCs | 2
           number of replicas a DC | 1
-----------------------------------+----------
 Manager RING hash
-----------------------------------+----------
                 current ring-hash | d5d667a6
                previous ring-hash | d5d667a6
-----------------------------------+----------

 [State of Node(s)]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | storage_0@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:55:35 +0900
  S    | storage_1@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:55:35 +0900
  S    | storage_2@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:55:35 +0900
  G    | gateway_0@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:55:37 +0900
-------+--------------------------+--------------+----------------+----------------+----------------------------

## 2. Remove a LeoStorage node
$ leofs-adm detach storage_0@127.0.0.1
OK

## 3. Launch a new LeoStorage node

## 4. Check the current state of the cluster(2)

$ leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.3.3
                        cluster Id | leofs_1
                             DC Id | dc_1
                    Total replicas | 2
          number of successes of R | 1
          number of successes of W | 1
          number of successes of D | 1
 number of rack-awareness replicas | 0
                         ring size | 2^128
-----------------------------------+----------
 Multi DC replication settings
-----------------------------------+----------
        max number of joinable DCs | 2
           number of replicas a DC | 1
-----------------------------------+----------
 Manager RING hash
-----------------------------------+----------
                 current ring-hash | d5d667a6
                previous ring-hash | d5d667a6
-----------------------------------+----------

 [State of Node(s)]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | storage_0@127.0.0.1      | detached     | d5d667a6       | d5d667a6       | 2017-04-18 18:56:32 +0900
  S    | storage_1@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:55:35 +0900
  S    | storage_2@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:55:35 +0900
  S    | storage_3@127.0.0.1      | attached     |                |                | 2017-04-18 18:56:47 +0900
  G    | gateway_0@127.0.0.1      | running      | d5d667a6       | d5d667a6       | 2017-04-18 18:55:37 +0900
-------+--------------------------+--------------+----------------+----------------+----------------------------

## 5. Execute `rebalance`
$ leofs-adm rebalance
Generating rebalance-list...
Generated rebalance-list
Distributing rebalance-list to the storage nodes
OK  33% - storage_2@127.0.0.1
OK  67% - storage_3@127.0.0.1
OK 100% - storage_1@127.0.0.1
OK

## 6. Check the latest state of the cluster
$ leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.3.3
                        cluster Id | leofs_1
                             DC Id | dc_1
                    Total replicas | 2
          number of successes of R | 1
          number of successes of W | 1
          number of successes of D | 1
 number of rack-awareness replicas | 0
                         ring size | 2^128
-----------------------------------+----------
 Multi DC replication settings
-----------------------------------+----------
        max number of joinable DCs | 2
           number of replicas a DC | 1
-----------------------------------+----------
 Manager RING hash
-----------------------------------+----------
                 current ring-hash | c613a468
                previous ring-hash | c613a468
-----------------------------------+----------

 [State of Node(s)]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | storage_1@127.0.0.1      | running      | c613a468       | c613a468       | 2017-04-18 18:55:35 +0900
  S    | storage_2@127.0.0.1      | running      | c613a468       | c613a468       | 2017-04-18 18:55:35 +0900
  S    | storage_3@127.0.0.1      | running      | c613a468       | c613a468       | 2017-04-18 18:58:16 +0900
  G    | gateway_0@127.0.0.1      | running      | c613a468       | c613a468       | 2017-04-18 18:55:37 +0900
-------+--------------------------+--------------+----------------+----------------+----------------------------
```


### Suspend a Node

When maintenance of a node is necessary, you can suspend a target node temporally. A suspended node does not receive requests from LeoGateway nodes and LeoStorage nodes. LeoFS eventually distributes the state of the cluster to every node.


```bash
## Example:
## 1. Execute `suspend`
$ leofs-adm suspend storage_1@127.0.0.1
OK


## 2. Check the latest state of the cluster
$ leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.3.3
                        cluster Id | leofs_1
                             DC Id | dc_1
                    Total replicas | 2
          number of successes of R | 1
          number of successes of W | 1
          number of successes of D | 1
 number of rack-awareness replicas | 0
                         ring size | 2^128
-----------------------------------+----------
 Multi DC replication settings
-----------------------------------+----------
        max number of joinable DCs | 2
           number of replicas a DC | 1
-----------------------------------+----------
 Manager RING hash
-----------------------------------+----------
                 current ring-hash | c613a468
                previous ring-hash | c613a468
-----------------------------------+----------

 [State of Node(s)]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | storage_1@127.0.0.1      | suspend      | c613a468       | c613a468       | 2017-04-18 18:55:35 +0900
  S    | storage_2@127.0.0.1      | running      | c613a468       | c613a468       | 2017-04-18 18:55:35 +0900
  S    | storage_3@127.0.0.1      | running      | c613a468       | c613a468       | 2017-04-18 18:58:16 +0900
  G    | gateway_0@127.0.0.1      | running      | c613a468       | c613a468       | 2017-04-18 18:55:37 +0900
-------+--------------------------+--------------+----------------+----------------+----------------------------
```

### Resume a Node

After suspending a node, if its node restarts and rejoins the cluster, execute `leofs-adm resume` command.

```bash
## Example:
## 1. Execute `resume`
$ leofs-adm resume storage_1@127.0.0.1
OK

## 2. Check the latest state of the cluster
$ leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.3.3
                        cluster Id | leofs_1
                             DC Id | dc_1
                    Total replicas | 2
          number of successes of R | 1
          number of successes of W | 1
          number of successes of D | 1
 number of rack-awareness replicas | 0
                         ring size | 2^128
-----------------------------------+----------
 Multi DC replication settings
-----------------------------------+----------
        max number of joinable DCs | 2
           number of replicas a DC | 1
-----------------------------------+----------
 Manager RING hash
-----------------------------------+----------
                 current ring-hash | c613a468
                previous ring-hash | c613a468
-----------------------------------+----------

 [State of Node(s)]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | storage_1@127.0.0.1      | running      | c613a468       | c613a468       | 2017-04-18 19:01:48 +0900
  S    | storage_2@127.0.0.1      | running      | c613a468       | c613a468       | 2017-04-18 18:55:35 +0900
  S    | storage_3@127.0.0.1      | running      | c613a468       | c613a468       | 2017-04-18 18:58:16 +0900
  G    | gateway_0@127.0.0.1      | running      | c613a468       | c613a468       | 2017-04-18 18:55:37 +0900
-------+--------------------------+--------------+----------------+----------------+----------------------------
```


## Related Links

- [For Administrators / Settings / Cluster Settings](../settings/cluster.md)
