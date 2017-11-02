# Systemd Services

This section describes how to manage LeoFS nodes on systemd-based Linux distributions.

## Requirements

Users of LeoFS v1.3.8 or higher running official Linux packages under supported systemd-based Linux distros (currently: Ubuntu 16.04 and EL7-based) can use supplied systemd units for starting/stopping nodes. Advantages offered by doing so include:

- Way to automatically start nodes on system startup and correctly shutdown them during power off / reboot.
- Reliable way of controlling nodes (e.g. when LeoStorage node is launched in absence of LeoManager nodes, it will keep on restarting till LeoManager becomes available; nodes always being able to restart in case of unexpected situation such as crash or OOM kill)
- Correct inter-node dependencies (affecting startup/shutdown order) for launching nodes in all-in-one cluster (when all nodes are running on the same system, e.g. for testing)

It's possible to switch to systemd-based way to control nodes (and back) anytime for users of v1.3.8 and higher running any supported systemd-based Linux distro.


## Switching to systemd services

For most users, switch is as easy as stopping currently running node and starting corresponding systemd service instead:

```
# /usr/local/leofs/current/leo_manager_0/bin/leo_manager stop
# systemctl start leofs-manager-master
```

Names of systemd units are: `leofs-manager-master`, `leofs-manager-slave`, `leofs-gateway`, `leofs-storage`. For example, to enable automatic startup of LeoStorage use command `systemctl enable leofs-storage`.

Immediate change when running nodes as systemd services is that `erlang.log` does not exist anymore; the information from that log file is available using journalctl, e.g.
```
# journalctl -u leofs-manager-master
```
(currently, default configuration of Ubuntu 16.04 and EL7 additionally makes these messages available in syslog like `/var/log/messages` or `/var/log/syslog`, but this depends on system settings)

## Caveats when using systemd services

### Mixing old and new way to launch nodes

Mixing launching / stopping nodes with launch scripts and systemd services is **not supported**. Please don't try to start node through systemd and then stop with launch script. However, some commands of current launch scripts (other than "start/stop/console/attach") are perfectly compatible with systemd services, for example:
```
# systemctl start leofs-storage
# /usr/local/leofs/current/leo_storage/bin/leo_storage ping
pong
# /usr/local/leofs/current/leo_storage/bin/leo_storage remote_console
Erlang/OTP 20 [erts-9.0] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V9.0  (abort with ^G)
```

There might be some places in LeoFS documentation that suggest you to execute launch scripts with start/stop commands. Please pay attention and substitute these with `systemctl start/stop` when running LeoFS nodes as systemd services.


### Running nodes as user other than `leofs`

Current systemd unit files try to launch LeoFS nodes as `leofs` user and group. Changing that through environment variable or environment config file won't work, because it's likely that `leofs` user does not have priviledge to further change user. In case running as different user is needed for some reason, please change service files using systemd's *override* feature like in this example:

```
# systemctl edit leofs-manager-master
```
Then enter
```
[Service]
User=<someuser>
Group=<somegroup>
```
Save and exit. Repeat for other unit files, if needed. For every node, user set in systemd unit files and in .environment config must be the same.


### Long shutdown times

In some cases (primary, LeoStorage under high write load) a node can take a long time to shutdown. When running node as systemd service, `reboot` command will ask node to shutdown safely before actually rebooting the system, which might take a long time under these conditions. Current limit for LeoStorage is set to 30 minutes, though this can be changed using systemd's override feature. Long shutdown / reboot times are expected for LeoStorage under high load.


### Running LeoFS nodes on systems running other Erlang software

This section only applies to people who need to run LeoFS node **and** some other Erlang software on the same system.

LeoFS, as well as certain types of other Erlang software relies on Erlang Port Mapper Daemon (`epmd`). Usually there can be only single instance of `epmd` running on the same node. To ensure smooth LeoFS operation, v1.3.8 and higher packages ensure that `leofs-epmd.socket` is enabled on boot and started right after upgrade. This is the case even when not using systemd-based services to launch LeoFS nodes. This service is running as "leofs" user by default (there should be no need to change that).

Usually, service for LeoFS-supplied `epmd` (which uses socket activation) should work even for other Erlang software that might need `epmd` and there will be no problems. However, in case running some other instance of `epmd` is really needed, it should be possible to mask `leofs-epmd.socket` and `leofs-epmd.service` and remove dependencies on these units. It should work as long as `epmd` is always running before launching LeoFS nodes and is set up to listen on all interfaces (`0.0.0.0`), not just the local one.
