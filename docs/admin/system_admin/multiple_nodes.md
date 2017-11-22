# Mutiple instances of nodes on the same system

## Description and requirements

Starting from v1.3.8, there is support for running multiple instances of the same node types on a single system (without use of containers or other type of isolation). This is useful for users who want to do things like

- Running two (or more) gateways on the same system (in the same or different mode, e.g. S3 gateway + REST gateway)
- Running two (or more) completely isolated clusters with different settings on the same set of storage servers

This feature currently works only on Linux. Other prerequisites for activating it are:

- Nodes must be launched as systemd services. See [For Administrators / System Operations / Systemd Services](/admin/system_operations/systemd.md) for details on enabling this feature.
- Nodes must load configuration from `/etc/leofs`. See [For Administrators / System Administration / Persistent Configuration Location](/admin/system_admin/persistent_configuration.md) for details on enabling this feature.
- **All paths and ports** in config files must be explicitly set for each instance and point to different locations. It's up to user to ensure that there are no conflicting paths.

This is advanced feature aimed at existing LeoFS users who need to run another cluster or more gateways using the same hardware; it is recommended to have a working cluster where all work and log directories are configured to use non-default locations before enabling this.

## Preparing more instances of the same node

Let's say we have LeoStorage instance running through systemd (`leofs-storage` service) and currently it loads configuration from `/etc/leofs/leo_storage`. We want to run another instance on the same system; it needs separate name. In this example, we'll call it "second". To create such instance:

1. Create local copy of systemd unit file with new name
    - EL7&nbsp;version: `cp /usr/lib/systemd/system/leofs-storage.service /etc/systemd/system/leofs-storage-second.service`.
    - Ubuntu&nbsp;16.04&nbsp;version: `cp /lib/systemd/system/leofs-storage.service /etc/systemd/system/leofs-storage-second.service`.
2. Edit `/etc/systemd/system/leofs-storage-second.service`, changing `Environment=NODE_EXTRA_NAME=` line to `Environment=NODE_EXTRA_NAME=second`.
3. Reload systemd configuration: `systemctl daemon-reload`.

First step here depends on distribution because default location of systemd unit file can differ; it can be checked in "UNIT FILE LOAD PATH" section of systemd.unit(5) man page.

This instance would try to load configuration from `/etc/leofs/leo_storage_second` upon launch. This configuration hierarchy needs to be created as well:

1. Create new configuration directory and *.d subdirectory: `mkdir -p /etc/leofs/leo_storage_second/leo_storage.d`.
2. Change permissions of configuration directory so it's **writable** by user running LeoFS ("leofs", unless changed manually): `chown leofs:leofs /etc/leofs/leo_storage_second`.
3. Copy main configuration file (if you plan to change it) or symlink it (if you only plan to use conf.d-style configuration override): `cp /etc/leofs/leo_storage/leo_storage.conf /etc/leofs/leo_storage_second/`.
4. Change configuration of new node, ensuring all paths and ports point to different location from first instance of that node.
5. Create directories (logs/queue/etc) for the new node and set appropriate permissions on them.

## List of options that must have different settings

All these options must be explicitly set in config files and have different settings for each instance of the node on the same system. For gateway and storage:
```
nodename

sasl.sasl_error_log
sasl.error_logger_mf_dir

log.erlang
log.app
log.member_dir
log.ring_dir

queue_dir
erlang.crash_dump
snmp_conf
```
For storage nodes, additionally:
```
rpc.server.listen_port
obj_containers.path
leo_ordning_reda.temp_stacked_dir
```
For gateways running in HTTP mode (S3 / REST), additionally:
```
http.port
http.ssl_port
```

Please note that this list might be not complete as new options might be added to config files. When using this feature, **it's up to user to ensure that all paths above point to different directories**. Trying to share the same ports or snmp_conf setting between nodes would result in second instance failing to start, but trying to share log or queue directory could lead to disastrous results. **Always double-check that all paths are different before using this feature.**

## Starting other node instances

After second instance is properly configured, it can be started in parallel with the first one with `systemctl start leofs-storage-second` command.

It is possible to check whether nodes actually try to load configuration from different directories ([described here in more detail](/admin/system_admin/persistent_configuration.md#how-to-check-which-configuration-directory-is-used)):
```
# journalctl -l -u leofs-storage | grep -e Exec -e Config
Sep 20 17:41:42 stor01.lan leo_storage[9513]: Config path: /etc/leofs/leo_storage
Sep 20 17:41:42 stor01.lan leo_storage[9513]: Exec: /usr/local/leofs/current/leo_storage/erts-9.0/bin/erlexec -noinput -boot /usr/local/leofs/current/leo_storage/releases/1/leo_storage -mode minimal -config /etc/leofs/leo_storage/app.config -args_file /etc/leofs/leo_storage/vm.args -- console

# journalctl -l -u leofs-storage-second | grep -e Exec -e Config
Sep 20 17:40:43 stor01.lan leo_storage[9065]: Config path: /etc/leofs/leo_storage_second
Sep 20 17:40:43 stor01.lan leo_storage[9065]: Exec: /usr/local/leofs/current/leo_storage/erts-9.0/bin/erlexec -noinput -boot /usr/local/leofs/current/leo_storage/releases/1/leo_storage -mode minimal -config /etc/leofs/leo_storage_second/app.config -args_file /etc/leofs/leo_storage_second/vm.args -- console
```

!!! note "Note: Multiple LeoManager instances"
    It is possible to run multiple instances of LeoManager as well, but the resulting configuration and `leofs-adm` operations would be more complicated, so it's not recommended (as results of error might be severe). It would be better to use separate VMs or containers for different LeoManagers.

!!! note "Note: Interacting with other instances through launch script"
    It is possible to interact with extra node instances through original launch script, however it requires a bit roundabout way of exporting environment variable as the owner of process. Example: `sudo -H -u leofs sh -c "export NODE_EXTRA_NAME=second; /usr/local/leofs/current/leo_storage/bin/leo_storage remote_console"`

## Example configuration

Here is fully working configuration example for two storage nodes belonging to two different clusters and working on the same system. Second instance is called "second" here (unit file for it is located at `/etc/systemd/system/leofs-storage-second.service` and it contains `Environment=NODE_EXTRA_NAME=second` setting).

Directories and permissions:
```
# ls -ld /etc/leofs/leo_storage /etc/leofs/leo_storage_second/
drwxr-sr-x. 3 leofs leofs 4096 Sep 22 20:47 /etc/leofs/leo_storage
drwxr-sr-x. 3 leofs leofs 4096 Sep 20 17:44 /etc/leofs/leo_storage_second/
```
Main configuration for second node is symlinked to first - preferred when configuring nodes through configuration override files. This allows automatically getting new default settings with package upgrades (which include new version of `/etc/leofs/leo_storage/leo_storage.conf`):
```
# ls -l /etc/leofs/leo_storage_second/leo_storage.conf
lrwxrwxrwx. 1 root leofs 31 Sep 19 21:09 /etc/leofs/leo_storage_second/leo_storage.conf -> ../leo_storage/leo_storage.conf
```

Contents of `/etc/leofs/leo_storage/leo_storage.d/20-leo_storage.conf` - configuration for the first node:
```
nodename = stor01@192.168.0.20
managers = [manager_0@192.168.0.10, manager_1@192.168.0.11]

obj_containers.path = [/mnt/avs/main]
obj_containers.num_of_containers = [64]

sasl.sasl_error_log = /var/log/leofs/leo_storage/sasl/sasl-error.log
sasl.error_logger_mf_dir = /var/log/leofs/leo_storage/sasl

log.erlang = /var/log/leofs/leo_storage/erlang
log.app = /var/log/leofs/leo_storage/app
log.member_dir = /var/log/leofs/leo_storage/ring
log.ring_dir = /var/log/leofs/leo_storage/ring

queue_dir  = /var/leofs/leo_storage/work/queue
leo_ordning_reda.temp_stacked_dir = /var/leofs/leo_storage/work/ord_reda/
erlang.crash_dump = /var/log/leofs/leo_storage/erl_crash.dump

snmp_conf = ./snmp/snmpa_storage_0/leo_storage_snmp
```

Contents of `/etc/leofs/leo_storage_second/leo_storage.d/20-leo_storage.conf` - configuration for second node
```
nodename = stor_second01@192.168.0.20
managers = [manager_0@192.168.0.15, manager_1@192.168.0.16]

rpc.server.listen_port = 13078

obj_containers.path = [/mnt/avs/second]
obj_containers.num_of_containers = [32]

object_storage.is_strict_check = true
watchdog.error.is_enabled = true
autonomic_op.compaction.is_enabled = true

sasl.sasl_error_log = /var/log/leofs/leo_storage_second/sasl/sasl-error.log
sasl.error_logger_mf_dir = /var/log/leofs/leo_storage_second/sasl

log.erlang = /var/log/leofs/leo_storage_second/erlang
log.app = /var/log/leofs/leo_storage_second/app
log.member_dir = /var/log/leofs/leo_storage_second/ring
log.ring_dir = /var/log/leofs/leo_storage_second/ring

queue_dir  = /var/leofs/leo_storage_second/work/queue
leo_ordning_reda.temp_stacked_dir = /var/leofs/leo_storage_second/work/ord_reda/
erlang.crash_dump = /var/log/leofs/leo_storage_second/erl_crash.dump

snmp_conf = ./snmp/snmpa_storage_1/leo_storage_snmp
```

Each node requires own set of directories with appropriate permissions:
```
# mkdir -p /var/leofs/leo_storage/work/queue /var/log/leofs/leo_storage/app /var/leofs/leo_storage/work/ord_reda /var/log/leofs/leo_storage/sasl
# chown -R leofs:leofs /var/leofs/leo_storage/ /var/log/leofs/
# mkdir -p /var/leofs/leo_storage_second/work/queue /var/log/leofs/leo_storage_second/app /var/leofs/leo_storage_second/work/ord_reda /var/log/leofs/leo_storage_second/sasl
# chown -R leofs:leofs /var/leofs/leo_storage_second/
```

## Related Links

- [For Administrators / System Operations / Systemd Services](/admin/system_operations/systemd.md)
- [For Administrators / System Administration / Persistent Configuration Location](/admin/system_admin/persistent_configuration.md)
