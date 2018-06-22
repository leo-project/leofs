# LeoManager's Maintenance

## Procedures of LeoManager's recovery

### Case 1: Recovery of a manager-master/manager-slave (not takeover)
- Stop both the manager-master and the manager-slave to avoid inconsistency of the data
```shell
$ leo_manager_<NUM>/bin/leo_manager stop
```
- Move `mnesia-files` which is under `work/mnesia/127.0.0.1` to another directory (backup)
```shell
$ mv leo_manager_1/work/mnesia/127.0.0.1/* <another-directory>
```
- Recover manager-master:
    - Copy `mnesia-files` from the manager-slave to the manager-master (not new one)
- Recover manager-slave:
    - Copy `mnesia-files` from the manager-master to the manager-slave (not new one)
```shell
$ cp <mnesia-files> leo_manager_<NUM>/work/mnesia/127.0.0.1/*
```
- Restart both the manager-master and the manager-slave
```shell
$ leo_manager_<NUM>/bin/leo_manager start
```

### Case 2: Launch a new manager-master/slave instead of a collapsed node (takeover)
- Export the mnesia-files with `leofs-adm dump-mnesia-data` command, which is output at a specified node, manager-master or manager-slave
```shell
## for manager-master
$ leofs-adm dump-mnesia-data <absolute-path>

## for manager-slave
$ leofs-adm dump-mnesia-data -p <port (default:10011)> <absolute-path>
```
- Stop both the manager-manager and the manager-slave
```shell
$ leo_manager_<NUM>/bin/leo_manager stop
```
- Move `mnesia-files` which is under `work/mnesia/127.0.0.1` to another directory (backup)
```shell
$ mv leo_manager_<NUM>/work/mnesia/127.0.0.1/* <another-directory>
```
- Update the configuration of `manager.partner` to be able to communicate between manager-master and manager-slave.
```ini
## Partner of manager's alias
manager.partner = manager_<NUM>@<IP>
```
- Restart both manager-master and manager-slave
```shell
$ leo_manager_<NUM>/bin/leo_manager start
```
- Import the mnesia-data from the back-up file
```shell
$ leofs-adm load-mnesia-data <absolute-path>
```
- Update the manager nodes to be able to communicate with the storage and the gateway nodes
```shell
$ leofs-adm update-managers manager_0@127.0.0.1 manager_2@127.0.0.1
OK
```
