# Persistent Configuration Location

## Node configuration in `/etc/leofs`

Starting from v1.3.8, LeoFS nodes support loading configuration from `/etc/leofs` hierarchy. This means that there are two locations from which nodes can load their configuration (but only one can be enabled at any time). Official Linux packages ship a copy of config files in `/etc/leofs/leo_*`, though the feature itself should work on any system. However, it is **disabled** by default for compatibility with older versions.

### Different configuration directory for each LeoFS version

**Currently, this is default for all installations.**

Config files are loaded from these locations when `/etc/leofs/leofs.conf` does not exist, is empty or contains `GLOBAL_CONFIG=no` (default). "%(version)" here is LeoFS version like "1.3.7":

| Node Type       | Full Path  |
| --------------- | ---------- |
| Manager Master  | /usr/local/leofs/%(version)/leo_manager_0/etc  |
| Manager Slave   | /usr/local/leofs/%(version)/leo_manager_1/etc  |
| Storage         | /usr/local/leofs/%(version)/leo_storage/etc    |
| Gateway         | /usr/local/leofs/%(version)/leo_gateway/etc    |

For each LeoFS version, these directories will be different (e.g. `/usr/local/leofs/1.3.6/leo_storage/etc` and `/usr/local/leofs/1.3.7/leo_storage/etc`). Because of this, config files needs to be copied into new directory after each upgrade.

!!! note "Note: Relative config files locations"
    The paths listed above work for default installation, but config files are actually loaded from directory relative to binaries. For example, if Manager Master is located in `/home/leofs/leo_manager_master`, config files will be picked from `/home/leofs/leo_manager_master/etc`.

### Same configuration directory for all versions

For LeoFS v1.3.8 or later, when `/etc/leofs/leofs.conf` exists and contains `GLOBAL_CONFIG=yes`, configuration is loaded from these directories - independent from version or binaries location:

| Node Type       | Full Path  |
| --------------- | ---------- |
| Manager Master  |/etc/leofs/leo_manager_0  |
| Manager Slave   |/etc/leofs/leo_manager_1  |
| Storage         |/etc/leofs/leo_storage    |
| Gateway         |/etc/leofs/leo_gateway    |

In this case config files don't need to be moved during upgrades. To enable this feature, edit `/etc/leofs/leofs.conf` file, setting
```
GLOBAL_CONFIG=yes
```

## Usage details

The new configuration directories apply to main config files and [environment files](/admin/settings/environment_config.md) (leo_manager.conf/.environment, leo_storage.conf/.environment, leo_gateway.conf/.environment). This **does not** apply to anything else by default, e.g. server_cert.pem/server_key.pem won't be picked from `/etc/leofs/leo_gateway` unless explicitly set so in leo_gateway.conf. It does not apply to .schema files as well, but this can be changed by setting `RUNNER_SCHEMA_DIR` in .environment file, if required.

Users have an option of either directly editing main config file (e.g.  `/etc/leofs/leo_gateway/leo_gateway.conf`), or creating a configuration override files in `/etc/leofs/leo_gateway/leo_gateway.d`, e.g. `/etc/leofs/leo_gateway/leo_gateway.d/local.conf`. If main config file was changed, all new package upgrades won't touch the changed file, leaving the task of merging new options and changed settings into current configuration to user. It might be a good idea to use configuration override files instead; if main config file was never edited, it will be replaced by newer version during package upgrades, while options that were changed will be loaded from configuration override directory after the main config file.

Example minimal configuration, override-style is a file `/etc/leofs/leo_gateway/leo_gateway.d/20-leo_gateway.conf` containing the following:
```
nodename = gateway_0@192.168.0.20
managers = [manager_0@192.168.0.10, manager_1@192.168.0.11]

protocol = s3
http.port = 8080

## Increase RAM cache
cache.cache_ram_capacity = 1073741824
```

## How to check which configuration directory is used

On launch, nodes report which directory they load config files from (also, the same directory is mentioned in "Exec" line of that output). When launching nodes through scripts directly, like `/usr/local/leofs/current/leo_manager_1/bin/leo_manager start` this information is available in erlang.log.* files. By default these files are located at `/usr/local/leofs/current/leo_*/log` (unless redefined with RUNNER_LOG_DIR= setting in environment config file). Please check which erlang.log.* file has the most recent date (it might be _not_ erlang.log.1). Example output:

```
$ grep -h -e Config -e Exec /usr/local/leofs/current/leo_manager_1/log/erlang.log.3
Config path: /etc/leofs/leo_manager_1
Exec: /usr/local/leofs/current/leo_manager_1/erts-8.3/bin/erlexec -heart -boot /usr/local/leofs/current/leo_manager_1/releases/1/leo_manager -mode embedded -config /etc/leofs/leo_manager_1/app.config -args_file /etc/leofs/leo_manager_1/vm.args -- console
```
This shows that configuration was loaded from `/etc/leofs/leo_manager_1`.

When launching nodes as [systemd services](/admin/system_operations/systemd.md), this information is stored in journald and available through journalctl:
```
$ journalctl -b -l -u leofs-manager-slave | grep -e Config -e Exec
Sep 29 19:45:42 leo-m1.dev leo_manager[19318]: Config path: /usr/local/leofs/current/leo_manager_1/etc
Sep 29 19:45:42 leo-m1.dev leo_manager[19318]: Exec: /usr/local/leofs/current/leo_manager_1/erts-9.0/bin/erlexec -noinput -boot /usr/local/leofs/current/leo_manager_1/releases/1/leo_manager -mode embedded -config /usr/local/leofs/current/leo_manager_1/etc/app.config -args_file /usr/local/leofs/current/leo_manager_1/etc/vm.args -- console
```
This shows that configuration was loaded from `/usr/local/leofs/current/leo_manager_1/etc` and persistent configuration directories in `/etc/leofs` are **not** used.


## Manual creation of /etc/leofs hierarchy

This section applies only to non-Linux users and Linux users who are not using official packages.

Configuration in `/etc/leofs` can be used even when it's not supplied with official packages; it should work if it's created properly. Here is example how to do it for LeoGateway:

1. Create directories: `mkdir -p /etc/leofs/leo_gateway`
2. Make sure that config directories are **writable** by user running LeoFS, e.g.  `chown leofs:leofs /etc/leofs/leo_gateway`. This is crucial, as nodes need to create extra files in that directory at launch.
3. Create copy of default configuration file, e.g. `cp /usr/local/leofs/%(version)/leo_gateway/etc/leo_gateway.conf /etc/leofs/leo_gateway/`
4. Optionally, create conf.d-style directories for changing only parts of configuration. E.g. `mkdir /etc/leofs/leo_gateway/leo_gateway.d`
5. Edit main config file (e.g. `/etc/leofs/leo_gateway/leo_gateway.conf`) or create configuration replacement file (e.g. `/etc/leofs/leo_gateway/leo_gateway.d/local.conf`). Optionally, create copy / edit environment file, if needed.
6. Create `/etc/leofs/leofs.conf` and write `GLOBAL_CONFIG=yes` in there.

## Related Links

- [For Administrators / Settings / Environment Configuration](/admin/settings/environment_config.md)
- [For Administrators / System Operations / Systemd Services](/admin/system_operations/systemd.md)
