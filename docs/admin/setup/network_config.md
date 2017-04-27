# Network Configurations
## Firewall Rules

In order for a LeoFS system to operate correctly, it is necessary to set and check the firewall rules in your environment. LeoFS depends on <a href="http://www.erlang.org/" target="_blank">Erlang/OTP</a>'s RPC, which uses specified ports and provides LeoFS' SNMP agent which also uses a port per a LeoFS' component node - LeoStorage, LeoGateway, and LeoManager.


| Subsystem           | Direction | Ports    | Description               |
|---------------------|-----------|---------:|---------------------------|
| **LeoManager (master)** |
| LeoManager (master) | Incoming  | 10010/*  | LeoManager console (text) |
| LeoManager (master) | Incoming  | 10020/*  | LeoManager console (json) |
| LeoManager (master) | Incoming  | 4369/*   | Erlang Port Mapper        |
| LeoManager (master) | Incoming  | 4020/*   | SNMP Listen Port          |
| LeoManager (master) | Outgoing  | */4369   | Erlang Port Mapper        |
| **LeoManager (slave)** |
| LeoManager (slave)  | Incoming  | 10011/*  | LeoManager console (text) |
| LeoManager (slave)  | Incoming  | 10021/*  | LeoManager console (json) |
| LeoManager (slave)  | Incoming  | 4369/*   | Erlang Port Mapper        |
| LeoManager (slave)  | Incoming  | 4021/*   | SNMP Listen Port          |
| LeoManager (slave)  | Outgoing  | */4369   | Erlang Port Mapper        |
| **LeoStorage**      |
| LeoStorage          | Incoming  | 4369/*   | Erlang Port Mapper        |
| LeoStorage          | Incoming  | 4010/*   | SNMP Listen Port          |
| LeoStorage          | Outgoing  | */4369   | Erlang Port Mapper        |
| **LeoGateway**      |
| LeoGateway          | Incoming  | 8080/*   | HTTP listen port          |
| LeoGateway          | Incoming  | 8443/*   | HTTPS listen port         |
| LeoGateway          | Incoming  | 4369/*   | Erlang Port Mapper        |
| LeoGateway          | Incoming  | 4000/*   | SNMP Listen Port          |
| LeoGateway          | Outgoing  | */4369   | Erlang Port Mapper        |

### How to Change Erlang's Port Range

Port range can be specified by setting Erlang's kernel variables `inet_dist_listen_min` and `inet_dist_listen_max`. If you need to change those configuration items, you need to enter the Erlang console of a target node.

```bash
## Example:
##   - This forces Erlang to use only ports 9100--9105 for distributed Erlang traffic.
$ bin/leo_storage remote_console

(storage_0@127.0.0.1)1> application:set_env(kernel, inet_dist_listen_min, 9100).
(storage_0@127.0.0.1)2> application:set_env(kernel, inet_dist_listen_max, 9105).

## Press [CTRL+c] to exit the Erlang console
```

## Related Links

- [For Administrators / Settings / LeoManager Settings](../settings/leo_manager.md)
- [For Administrators / Settings / LeoStorage Settings](../settings/leo_storage.md)
- [For Administrators / Settings / LeoGateway Settings](../settings/leo_gateway.md)
