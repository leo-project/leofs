# Settings
## Configuration Fundamentals
### LeoFS' Configuration Files

Each LeoFS' component has a configuration file in a package:

* <a href="https://github.com/leo-project/leofs/blob/master/apps/leo_manager/priv/leo_manager_0.conf" target="_blank">LeoManager master's configuration</a>
* <a href="https://github.com/leo-project/leofs/blob/master/apps/leo_manager/priv/leo_manager_1.conf" target="_blank">LeoManager slave's configuration</a>
* <a href="https://github.com/leo-project/leofs/blob/master/apps/leo_storage/priv/leo_storage.conf" target="_blank">LeoStorage's configuration</a>
* <a href="https://github.com/leo-project/leofs/blob/master/apps/leo_gateway/priv/leo_gateway.conf" target="_blank">LeoGateway's configuration</a>

LeoFS' configuration filesare are located in the following directories:

| Application    | Location |
|---             |---       |
|LeoManger master|`{LEOFS_ROOT}/leo_manager_0/etc/leo_manager.conf`|
|LeoManger slave |`{LEOFS_ROOT}/leo_manager_1/etc/leo_manager.conf`|
|LeoStorage      |`{LEOFS_ROOT}/leo_storage/etc/leo_storage.conf`  |
|LeoGateway      |`{LEOFS_ROOT}/leo_storage/etc/leo_storage.conf`  |


The `*.config` file is used to set various attributes of a LeoFS storage system. It also used to pass parameters to the Erlang node such as the name and cookie of the node.


## Contents of Settings

* [Cluster Settings](cluster.md)
* [LeoManager Settings](leo_manager.md)
* [LeoStorage Settings](leo_storage.md)
* [LeoGateway Settings](leo_gateway.md)
* [Watchdog Settings](watchdog.md)