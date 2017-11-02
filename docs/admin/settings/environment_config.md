# Environment Config Files
## Overview

Starting from v1.3.3, some environment variables used by launch scripts can be redefined in
**"environment config files"**. They have shell syntax and are read by launch scripts.

**[Environment Files]**

```
leo_manager_0/etc/leo_manager.environment
leo_manager_1/etc/leo_manager.environment
leo_gateway/etc/leo_gateway.environment
leo_storage/etc/leo_storage.environment
```

Changing settings in these files is completely optional, but can be used to better organize directories used by LeoFS nodes and simplify upgrades. Here is highly customized example of `.environment` and `.config` files that allow LeoFS to store all work information and logs outside of default installation tree (`/usr/local/leofs/<version>`).

As a result, upgrade process to newer version becomes as simple as placing `leo_*.environment` files in `etc` directory of new version, for example (for `leo_manager_0`):

```bash
## Stop the process of LeoManager
$ /usr/local/leofs/<old_version>/leo_manager_0/bin/leo_manager stop

## Overwrite the environment file
$ cp /usr/local/leofs/<old_version>/leo_manager_0/etc/leo_manager.environment \
     /usr/local/leofs/<new_version>/leo_manager_0/etc/

## Restart the process of LeoManager
$ /usr/local/leofs/<new_version>/leo_manager_0/bin/leo_manager start
```

With this, users can place actual config files (like `leo_manager.conf`) to the directory of their choice and change them independently of version upgrades, and the `.environment` files that need to be placed into installation tree don't need to be changed between versions. With the correct setup, since no work/temporary files will be kept in the installation tree, old version can be removed cleanly.

!!! note "Note: When managing nodes through systemd"
    Please do not use example commands above (`leo_manager stop/start`) when running nodes as systemd services for LeoFS v1.3.8 or later, use `systemctl stop/start leofs-manager-master` instead.


## Example Configuration

Contents of `/usr/local/leofs/<version>/leo_manager_0/etc/leo_manager.environment`:

```ini
# pick config file from fixed place
RUNNER_ETC_DIR=/etc/leofs/leo_manager_0
# store erlang.log.* and run_erl.log in this directory
RUNNER_LOG_DIR=/var/log/leofs/leo_manager_0
```

Directories defined in `RUNNER_ETC_DIR` and `RUNNER_LOG_DIR` *(in this example, `/etc/leofs/leo_manager_0` and `/var/log/leofs/leo_manager_0`)* must be writable by `leofs` user, also `$RUNNER_LOG_DIR/sasl` *(here `/var/log/leofs/leo_manager_0/sasl`)* must exist:

```
drwxr-xr-x. 2 leofs leofs 4096 Apr  4 20:40 /etc/leofs/leo_manager_0/
drwxr-xr-x. 4 leofs leofs 4096 Apr  5 20:00 /var/log/leofs/leo_manager_0/
drwxr-xr-x. 2 leofs leofs 4096 Apr  4 20:40 /var/log/leofs/leo_manager_0/sasl/
```

Paths containing whitespace characters (spaces, tabs, etc) aren't allowed in environment files due to technical reasons.

In `leo_manager.conf`, all options related to directories should point to external paths:

```ini
sasl.sasl_error_log = /var/log/leofs/leo_manager_0/sasl/sasl-error.log
sasl.error_logger_mf_dir = /var/log/leofs/leo_manager_0/sasl
mnesia.dir = /var/local/leofs/leo_manager_0/work/mnesia/127.0.0.1
queue_dir = /var/local/leofs/leo_manager_0/work/queue
log.erlang = /var/log/leofs/leo_manager_0/erlang
log.app = /var/log/leofs/leo_manager_0/app
log.member_dir = /var/log/leofs/leo_manager_0/ring
log.ring_dir = /var/log/leofs/leo_manager_0/ring
erlang.crash_dump = /var/log/leofs/leo_manager_0/erl_crash.dump
```

For `leo_storage.conf` it will be:

```ini
sasl.sasl_error_log = /var/log/leofs/leo_storage/sasl/sasl-error.log
sasl.error_logger_mf_dir = /var/log/leofs/leo_storage/sasl
obj_containers.path = [/mnt/avs]
log.erlang = /var/log/leofs/leo_storage/erlang
log.app = /var/log/leofs/leo_storage/app
log.member_dir = /var/log/leofs/leo_storage/ring
log.ring_dir = /var/log/leofs/leo_storage/ring
queue_dir  = /var/local/leofs/leo_storage/work/queue
leo_ordning_reda.temp_stacked_dir = /var/local/leofs/leo_storage/work/ord_reda/
erlang.crash_dump = /var/log/leofs/leo_storage/erl_crash.dump
```

For `leo_gateway.conf`:

```ini
sasl.sasl_error_log = /var/log/leofs/leo_gateway/sasl/sasl-error.log
sasl.error_logger_mf_dir = /var/log/leofs/leo_gateway/sasl
log.erlang = /var/log/leofs/leo_gateway/erlang
log.app = /var/log/leofs/leo_gateway/app
log.member_dir = /var/log/leofs/leo_gateway/ring
log.ring_dir = /var/log/leofs/leo_gateway/ring
cache.cache_disc_dir_data = /var/local/leofs/leo_gateway/cache/data
cache.cache_disc_dir_journal = /var/local/leofs/leo_gateway/cache/journal
queue_dir = /var/local/leofs/leo_gateway/work/queue
erlang.crash_dump = /var/log/leofs/leo_gateway/erl_crash.dump
```

All these directories must exist and have correct **ownership/permissions** *(writable by `leofs` user, unless set up otherwise)*


## Additional settings - SNMP config

When pursuing "pure" system which keeps all the data out of installation tree, one might also decide to move SNMP agent config and `SNMP db directories` to external paths, by setting it in `leo_manager.config`:

```ini
## leo_manager_0.conf
snmp_conf = /etc/leofs/leo_manager_0/leo_manager_snmp
```

then copying `/usr/local/leofs/<version>/leo_manager_0/snmp/snmpa_manager_0/leo_manager_snmp.config` to `/etc/leofs/leo_manager_0/leo_manager_snmp` and setting

```erlang
{db_dir, "/var/local/leofs/leo_manager_0/snmp_db"},
```

in `/etc/leofs/leo_manager_0/leo_manager_snmp.config` to make sure that absolutely no temporary files are created in `/usr/local/leofs` tree. It shouldn't matter otherwise since there is no need to keep contents of `SNMP db directory` between upgrades.

*(Here, copy of leo_manager_snmp.config was made so that original config would be untouched; while it is possible to change `db_dir` in original `/usr/local/leofs/<version>/leo_manager_0/snmp/snmpa_manager_0/leo_manager_snmp.config` as well, doing so would mean that this file needs to be replaced after each upgrade, reducing benefit of only changing `.environment` file after upgrade)*


## Notice

Note that this configuration is just an example of how to use `.environment` config features to move all the log files and config files out of the tree so they reside at fixed paths, to simplify configuration changes and upgrades as much as possible.

The resulting upgrade process can be less safe than original one suggested at [For Administrators / System Administration / System Migration](/admin/system_admin/migration.md), because the new version changes working `mnesia` and `queue` directories upon launch and going back to the older version might be not always possible.

Users should consider making backups of work directories (`/var/local/leofs` in this example) before launching the newer version of a node.


## Related Links

- [For Administrators / System Administration / System Migration](/admin/system_admin/migration.md)
