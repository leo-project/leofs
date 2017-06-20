# Settings
## Configuration Fundamentals
### LeoFS' Configuration Files

Each LeoFS' component has a configuration file in a package:

* <a href="https://github.com/leo-project/leofs/blob/master/apps/leo_manager/priv/leo_manager_0.conf" target="_blank">LeoManager master's configuration</a>
* <a href="https://github.com/leo-project/leofs/blob/master/apps/leo_manager/priv/leo_manager_1.conf" target="_blank">LeoManager slave's configuration</a>
* <a href="https://github.com/leo-project/leofs/blob/master/apps/leo_storage/priv/leo_storage.conf" target="_blank">LeoStorage's configuration</a>
* <a href="https://github.com/leo-project/leofs/blob/master/apps/leo_gateway/priv/leo_gateway.conf" target="_blank">LeoGateway's configuration</a>

LeoFS' configuration files are located in the following directories:

| Application    | Location |
|---             |---       |
|LeoManager master           |`{LEOFS_ROOT}/leo_manager_0/etc/leo_manager.conf`|
|LeoManager master (optional)|`{LEOFS_ROOT}/leo_manager_0/etc/leo_manager.d/*.conf`|
|LeoManager slave            |`{LEOFS_ROOT}/leo_manager_1/etc/leo_manager.conf`|
|LeoManager slave (optional) |`{LEOFS_ROOT}/leo_manager_1/etc/leo_manager.d/*.conf`|
|LeoStorage                  |`{LEOFS_ROOT}/leo_storage/etc/leo_storage.conf`  |
|LeoStorage (optional)       |`{LEOFS_ROOT}/leo_storage/etc/leo_storage.d/*.conf`  |
|LeoGateway                  |`{LEOFS_ROOT}/leo_gateway/etc/leo_gateway.conf`  |
|LeoGateway (optional)       |`{LEOFS_ROOT}/leo_gateway/etc/leo_gateway.d/*.conf`  |


The `*.conf` file is used to set various attributes of a LeoFS storage system. It also used to pass parameters to the Erlang node such as the name and cookie of the node.

Optional configuration files in `*.d/*.conf` are for additional configuration, in case system administrator prefers to keep all the changed settings in separate config and keep config with default settings unchanged (so that config merging is not required during version upgrade which changes default config files). If these files exist, they are processed (in alphabetical order) after the main configuration file. For each option in config files, the last defined value is used. Due to technical reasons, config file names or full paths to them shouldn't contain whitespace characters (spaces, tabs, etc).


## Contents of Settings

* [Cluster Settings](cluster.md)
* [LeoManager Settings](leo_manager.md)
* [LeoStorage Settings](leo_storage.md)
* [LeoGateway Settings](leo_gateway.md)
* [Watchdog Settings](watchdog.md)
