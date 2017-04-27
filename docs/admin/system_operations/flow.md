# Launch and Operation Flow

## Operation Flow

1. Build a LeoManager cluster
2. Confirm the state of the LeoManager cluster
3. Build a LeoStorage cluster
4. Confirm the state of the LeoStorage cluster
5. Launch LeoGateway nodes
6. Confirm the state of the LeoFS system
7. After launching the system, execute processing depending on the status of the system
    - [Attach a LeoStorage node](/admin/system_operations/cluster/#add-a-node)
    - [Detach a LeoStorage node](/admin/system_operations/cluster/#remove-a-node)
    - [Suspend a LeoStorage node](/admin/system_operations/cluster/#suspend-a-node)
    - [Resume a LeoStorage node](/admin/system_operations/cluster/#resume-a-node)

![](../../assets/leofs-flow-diagram.jpg)

- <a href="../../../assets/leofs-flow-diagram.jpg" target="_blank">The diagram only</a>


## Launch Order of LeoFS' Components

![](../../assets/leofs-order-of-system-launch.png)

| Command                        | Description |
|--------------------------------|-------------|
| **LeoManager** |
| 1. `$ leofs_manager start`     | Start LeoManager’s master |
| 2. `$ leofs_manager start`     | Start LeoManager’s slave  |
| **LeoManager** |
| 3. `$ leofs_storage start`     | Start a LeoStorage node   |
| (*Repeatedly launch LeoStorage nodes*) |
| 4. `$ leofs-adm start`         | Start Leostorage cluster  |
| 5. `$ leofs-adm status`        | Confirm the current state of the LeoFS system (1) |
| **LeoGateway** |
| 6. `$ bin/leofs_gateway start` | Start a LeoGateway node |
| (*Repeatedly launch LeoGateway nodes*) |
| 7. `$ leofs-adm status`        | Confirm the current state of the LeoFS system (2) |


## Related Links

- [Getting Started / Quick Installation and Setup](/installation/quick.md)
- [Getting Started / Building a LeoFS' cluster with Ansible](/installation/cluster.md)
- [For Administrators / System Operations / Cluster Operations](/admin/system_operations/cluster.md)
