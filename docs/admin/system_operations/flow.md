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
| `$ leofs_manager start` | Start LeoManager’s master |
| `$ leofs_manager start` | Start LeoManager’s slave  |
| **LeoManager** |
| `$ leofs_storage start` | Start a LeoStorage node   |
| (*Repeatedly launch LeoStorage nodes*) |
| `$ leofs-adm start` | Start Leostorage cluster  |
| `$ leofs-adm status` | Confirm the current state of the LeoFS system (1) |
| **LeoGateway** |
| `$ bin/leofs_gateway start` | Start a LeoGateway node |
| (*Repeatedly launch LeoGateway nodes*) |
| `$ leofs-adm status` | Confirm the current state of the LeoFS system (2) |

!!! note "Note: Restart a LeoManager when both of them are down"
    When both of the LeoManagers are down and you try to restart a LeoManager that is NOT the one terminated at last, you can not restart the LeoManager because a sprit-brain could happen. If you make sure there is no case the data inconsistency could happen due to a sprit-brain then do `leo_manager start force_load` that allows you to restart a LeoManager in such cases. Please refer [What is the significance of a Mnesia Master Node in a cluster](http://stackoverflow.com/questions/3573404/what-is-the-significance-of-a-mnesia-master-node-in-a-cluster) for more information.

## Related Links

- [Getting Started / Quick Installation and Setup](/installation/quick.md)
- [Getting Started / Building a LeoFS' cluster with Ansible](/installation/cluster.md)
- [For Administrators / System Operations / Cluster Operations](/admin/system_operations/cluster.md)
