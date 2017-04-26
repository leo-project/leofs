# Migration
## Prior Knowledge

## Procedure For Upgrading

## Note
### Since 1.3.3
### For those who already have run LeoFS with <= 1.3.2.1
### Overview
Starting from 1.3.3, all LeoFS nodes are running as non-privileged user `leofs` in official Linux packages.
It should work out of the box for new installations and for new nodes on existing installations.
However, for existing nodes upgrading to 1.3.3 (or later) from earlier versions the change might be not seamless. Compared to the usual upgrade procedure described at [System Maintenance](http://leo-project.net/leofs/docs/admin_guide/admin_guide_10.html),
extra steps are needed. There are a few options, depending on how the node was configured.

### Extra Steps
#### Running LeoFS with default paths
For those who have LeoFS configured with
- queue and mnesia in `/usr/local/leofs/<version>/leo_*/work`
- log files in `/usr/local/leofs/<version>/leo_*/log`
- storage data files in `/usr/local/leofs/<version>/leo_storage/avs`

Follow the below instructions when upgrading.

1. During upgrade of node (of any type), **after** stopping the old version and copying or moving every
files to be moved into the new directories, change the owner with the commands below. It has to be done **before** launching the
new version.
```
# chown -R leofs:leofs /usr/local/leofs/%version/leo_storage/avs
# chown -R leofs:leofs /usr/local/leofs/%version/leo_gateway/cache
# chown -R leofs:leofs /usr/local/leofs/%version/leo_*/log
# chown -R leofs:leofs /usr/local/leofs/%version/leo_*/work
```

2. Remove old temporary directory used by launch scripts. This step is needed because when earlier version was launched with `root` permissions, it creates a set of temporary directories in `/tmp` which cannot be re-used by non-privileged user as is, and launch scripts will fail with obscure messages - or with no message at all, except for an error in syslog (usually `/var/log/messages`).
```
# rm -rf /tmp/usr
```

3. Start the node through its launch script, as per upgrade flow diagram.

#### Running LeoFS with customized paths
For those who have LeoFS configured with, for example, let's say
- queue and mnesia in `/mnt/work`
- log files in `/var/log/leofs`
- storage data files in `/mnt/avs`

1. Before starting new version of a node, execute `chown -R leofs:leofs <..>` for all these external directories

2. Don't forget to remove temporary directory (`rm -rf /tmp/usr`) as well for the reasons described above.

These users might be interested in new features of "environment" config files, which allow to redefine
some environment variables like paths in launch script. Please refere [For Administrators / Settings / Environment Configuration](/admin/settings/environment_config.md) for more information.

#### Running LeoFS with customized launch scripts
For those who have LeoFS already running as non-privileged user.

1. Scripts that are provided by packages generally should be enough to run on most configurations without changes. If needed, change user from `leofs` to some other in "environment" config files (e.g. `RUNNER_USER=localuser`). Please refer to the later section for more details about environment config files.

2. Possible pitfall includes ownership of `/usr/local/leofs/.erlang.cookie` file, which is set to `leofs` during package installation. This should only be a problem when trying to run LeoFS nodes with permissions of some user which is not called `leofs`, but has home directory set to `/usr/local/leofs`. This is not supported due to technical reasons. Home directory of that user must be set to something else.

#### Running LeoFS in any form and keeping LeoFS running as `root`
For those who want to keep maximum compatibility with the previous installation.

1. In "environment" config file, set this option
```
RUNNER_USER=root
```

Please note that switching this node to run as non-privileged user later will require extra steps to carefully change all permissions. This is not recommended, but possible (at very least, in addition to `chown` commands from before, permissions of `leo_*/etc` and `leo_*/snmp/*/db` will have to be changed recursively as well).

## Note for Developers
As described at the previous section, The default user running LeoFS processes has changed to `leofs` so that requires developers to

- Tweak environment config files to set **RUNNER_USER** to the user you have logged in while developing with **make release/bootstrap.sh/mdcr.sh**.
- Remove all files under **$PIPE_DIR** before starting any LeoFS processes.

## Related Links
- [For Administrators / Settings / Environment Configuration](/admin/settings/environment_config.md)
