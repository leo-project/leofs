# System Monitoring
## SNMPA Setup

Each node of LeoStorage, LeoGateway and LeoManager provides a built in SNMP agent which allows to connect external systems, such as <a href="https://www.nagios.com/" target="_blank">Nagios</a> and <a href="https://www.zabbix.com/" target="_blank">Zabbix</a>. You can retrieve various statistics of your LeoFS.

### LeoManager
#### SNMPA Properties

| Item | Value / Range |
|------|---------------|
| Port | 4020..4021, 14020..14021 |
| Branch | 1.3.6.1.4.1.35450 |
| [snmpa\_manager\_0](https://github.com/leo-project/leofs/tree/master/apps/leo_manager/snmp/snmpa_manager_0) | Port: 4020 |
| [snmpa\_manager\_1](https://github.com/leo-project/leofs/tree/master/apps/leo_manager/snmp/snmpa_manager_1) | Port: 4021 |
| [snmpa\_manager\_2](https://github.com/leo-project/leofs/tree/master/apps/leo_manager/snmp/snmpa_manager_2) | Port: 14020 |
| [snmpa\_manager\_3](https://github.com/leo-project/leofs/tree/master/apps/leo_manager/snmp/snmpa_manager_3) | Port: 14021 |

#### SNMPA Items of Erlang-VM

| Branch Number | Description |
|---------------|-------------|
| 1             | Node name|
| **1 min average** |
| 2             | Total numeber of processes |
| 3             | Total memory usage |
| 4             | System memory usage |
| 5             | Processes memory usage |
| 6             | ETS memory usage |
| **5 min average** |
| 7             | Total numeber of processes |
| 8             | Total memory usage |
| 9             | System memory usage |
| 10            | Processes memory usage |
| 11            | ETS memory usage |
| **Allocated memmory** |
| 12            | Used/allocated memory for 1 min |
| 13            | Allocated memory for 1 min |
| 14            | Used/allocated memory for 5 min |
| 15            | Allocated memory for 5 min |

#### Check the configuration with `snmpwalk` command after starting LeoFS

```bash
$ snmpwalk -v 2c -c public 127.0.0.1:4020 .1.3.6.1.4.1.35450
SNMPv2-SMI::enterprises.35450.15.1.0 = STRING: "manager_0@127.0.0.1"
SNMPv2-SMI::enterprises.35450.15.2.0 = Gauge32: 123
SNMPv2-SMI::enterprises.35450.15.3.0 = Gauge32: 30289989
SNMPv2-SMI::enterprises.35450.15.4.0 = Gauge32: 24256857
SNMPv2-SMI::enterprises.35450.15.5.0 = Gauge32: 6033132
SNMPv2-SMI::enterprises.35450.15.6.0 = Gauge32: 1914017
SNMPv2-SMI::enterprises.35450.15.7.0 = Gauge32: 123
SNMPv2-SMI::enterprises.35450.15.8.0 = Gauge32: 30309552
SNMPv2-SMI::enterprises.35450.15.9.0 = Gauge32: 24278377
SNMPv2-SMI::enterprises.35450.15.10.0 = Gauge32: 6031175
SNMPv2-SMI::enterprises.35450.15.11.0 = Gauge32: 1935758
SNMPv2-SMI::enterprises.35450.15.12.0 = Gauge32: 75
SNMPv2-SMI::enterprises.35450.15.13.0 = Gauge32: 84635402
SNMPv2-SMI::enterprises.35450.15.14.0 = Gauge32: 78
SNMPv2-SMI::enterprises.35450.15.15.0 = Gauge32: 88735915
```

### LeoStorage
#### SNMPA Properties

| Item | Value / Range |
|------|---------------|
| Port | 4010..4014 |
| Branch | 1.3.6.1.4.1.35450 |
| [snmpa\_storage\_0](https://github.com/leo-project/leofs/tree/master/apps/leo_storage/snmp/snmpa_storage_0) | Port: 4010 |
| [snmpa\_storage\_1](https://github.com/leo-project/leofs/tree/master/apps/leo_storage/snmp/snmpa_storage_1) | Port: 4011 |
| [snmpa\_storage\_2](https://github.com/leo-project/leofs/tree/master/apps/leo_storage/snmp/snmpa_storage_2) | Port: 4012 |
| [snmpa\_storage\_3](https://github.com/leo-project/leofs/tree/master/apps/leo_storage/snmp/snmpa_storage_3) | Port: 4013 |

#### SNMPA Items of Erlang-VM

| Branch Number | Description |
|---------------|-------------|
| 1             | Node name|
| **1 min average** |
| 2             | Total numeber of processes |
| 3             | Total memory usage |
| 4             | System memory usage |
| 5             | Processes memory usage |
| 6             | ETS memory usage |
| **5 min average** |
| 7             | Total numeber of processes |
| 8             | Total memory usage |
| 9             | System memory usage |
| 10            | Processes memory usage |
| 11            | ETS memory usage |
| **Allocated memmory** |
| 31            | Used/allocated memory for 1 min |
| 32            | Allocated memory for 1 min |
| 33            | Used/allocated memory for 5 min |
| 34            | Allocated memory for 5 min |

#### SNMPA Items of LeoStorage

| Branch Number | Description |
|---------------|-------------|
| **Request counter for 1 min** |
| 12            | Total number of WRITE requests |
| 13            | Total number of READ requests |
| 14            | Total number of DELETE requests |
| **Request counter for 5 min** |
| 15            | Total number of WRITE requests |
| 16            | Total number of READ requests |
| 17            | Total number of DELETE requests |
| **Stored objects related** |
| 18            | Total number of active objects |
| 19            | Total number of objects<br/> *(It includes inactive objects which are removed and updated objects)* |
| 20            | Total active object size |
| 21            | Total object size |
| **MQ related** |
| 22            | Total messages of `object replication` |
| 23            | Total messages of `sync-vnodes` |
| 24            | Total messages of `rebalance`   |
| 41            | Total messages of `recovery-node` *(since `v1.4.0`)* |
| 42            | Total messages of `deletion-directry` *(since `v1.4.0`)* |
| 43            | Total messages of `async deletion-directries` *(since `v1.4.0`)* |
| 44            | Total messages of a requet of `deletion-directry` *(since `v1.4.0`)* |
| 45            | Total messages of `comparison-metadata` for the multi datacenter replication *(since `v1.4.0`)* |
| 46            | Total messages of a request of `sync-object` for the multi datacenter replication *(since `v1.4.0`)* |
| **Data-compaction related** |
| 51            | data-compaction state<br/><ul><li>0: `idling`</li><li>1: `running`</li><li>2: `suspending`</li></ul> |
| 52            | Start date time of last data-compaction *(unixtime)* |
| 53            | End date time of last data-compaction *(unixtime)* |
| 54            | Total number of pending targets |
| 55            | Total number of ongoing targets |
| 56            | Total number of out of targets  |

#### Check the configuration with `snmpwalk` command after starting LeoFS

```
$ snmpwalk -v 2c -c public 127.0.0.1:4010 .1.3.6.1.4.1.35450
iso.3.6.1.4.1.35450.56.1.0 = STRING: "storage_0@127.0.0.1"
iso.3.6.1.4.1.35450.56.2.0 = Gauge32: 577
iso.3.6.1.4.1.35450.56.3.0 = Gauge32: 47509309
iso.3.6.1.4.1.35450.56.4.0 = Gauge32: 27404799
iso.3.6.1.4.1.35450.56.5.0 = Gauge32: 20096683
iso.3.6.1.4.1.35450.56.6.0 = Gauge32: 5967268
iso.3.6.1.4.1.35450.56.7.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.8.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.9.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.10.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.11.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.12.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.13.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.14.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.15.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.16.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.17.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.18.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.19.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.20.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.21.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.22.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.23.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.24.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.31.0 = Gauge32: 63
iso.3.6.1.4.1.35450.56.32.0 = Gauge32: 73028949
iso.3.6.1.4.1.35450.56.33.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.34.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.41.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.42.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.43.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.44.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.45.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.46.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.51.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.52.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.53.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.54.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.55.0 = Gauge32: 0
iso.3.6.1.4.1.35450.56.56.0 = Gauge32: 0
```

### LeoGateway
#### SNMPA Properties

| Item | Value / Range |
|------|---------------|
| Port | 4000..4001    |
| Branch | 1.3.6.1.4.1.35450 |
| [snmpa\_gateway\_0](https://github.com/leo-project/leofs/tree/master/apps/leo_gateway/snmp/snmpa_gateway_0) | Port: 4000 |
| [snmpa\_gateway\_1](https://github.com/leo-project/leofs/tree/master/apps/leo_gateway/snmp/snmpa_gateway_1) | Port: 4001 |

#### SNMPA Items of Erlang-VM

| Branch Number | Description |
|---------------|-------------|
| 1             | Node name|
| **1 min average** |
| 2             | Total numeber of processes |
| 3             | Total memory usage |
| 4             | System memory usage |
| 5             | Processes memory usage |
| 6             | ETS memory usage |
| **5 min average** |
| 7             | Total numeber of processes |
| 8             | Total memory usage |
| 9             | System memory usage |
| 10            | Processes memory usage |
| 11            | ETS memory usage |
| **Allocated memmory** |
| 31            | Used/allocated memory for 1 min |
| 32            | Allocated memory for 1 min |
| 33            | Used/allocated memory for 5 min |
| 34            | Allocated memory for 5 min |

#### SNMPA Items of LeoGateway

| Branch Number | Description |
|---------------|-------------|
| **Request counter for 1 min** |
| 12 | Total number of WRITE requests |
| 13 | Total number of READ requests |
| 14 | Total number of DELETE requests |
| **Request counter for 5 min** |
| 15 | Total number of WRITE requests |
| 16 | Total number of READ requests |
| 17 | Total number of DELETE requests |
| **Cache related** |
| 18 | Total counts of cache-hit |
| 19 | Total counts of cache-miss |
| 20 | Total number of cached objects |
| 21 | Total cached object size |

#### Check the configuration with `snmpwalk` command after starting LeoFS

```
$ snmpwalk -v 2c -c public 127.0.0.1:4000 .1.3.6.1.4.1.35450
SNMPv2-SMI::enterprises.35450.34.1.0 = STRING: "gateway_0@127.0.0.1"
SNMPv2-SMI::enterprises.35450.34.2.0 = Gauge32: 279
SNMPv2-SMI::enterprises.35450.34.3.0 = Gauge32: 45266128
SNMPv2-SMI::enterprises.35450.34.4.0 = Gauge32: 36653905
SNMPv2-SMI::enterprises.35450.34.5.0 = Gauge32: 8612223
SNMPv2-SMI::enterprises.35450.34.6.0 = Gauge32: 2276519
SNMPv2-SMI::enterprises.35450.34.7.0 = Gauge32: 279
SNMPv2-SMI::enterprises.35450.34.8.0 = Gauge32: 45157433
SNMPv2-SMI::enterprises.35450.34.9.0 = Gauge32: 36385227
SNMPv2-SMI::enterprises.35450.34.10.0 = Gauge32: 8772210
SNMPv2-SMI::enterprises.35450.34.11.0 = Gauge32: 2261105
SNMPv2-SMI::enterprises.35450.34.12.0 = Gauge32: 0
SNMPv2-SMI::enterprises.35450.34.13.0 = Gauge32: 13
SNMPv2-SMI::enterprises.35450.34.14.0 = Gauge32: 0
SNMPv2-SMI::enterprises.35450.34.15.0 = Gauge32: 3
SNMPv2-SMI::enterprises.35450.34.16.0 = Gauge32: 24
SNMPv2-SMI::enterprises.35450.34.17.0 = Gauge32: 0
SNMPv2-SMI::enterprises.35450.34.18.0 = Gauge32: 21
SNMPv2-SMI::enterprises.35450.34.19.0 = Gauge32: 39
SNMPv2-SMI::enterprises.35450.34.20.0 = Gauge32: 3
SNMPv2-SMI::enterprises.35450.34.21.0 = Gauge32: 565700
SNMPv2-SMI::enterprises.35450.34.31.0 = Gauge32: 75
SNMPv2-SMI::enterprises.35450.34.32.0 = Gauge32: 84635402
SNMPv2-SMI::enterprises.35450.34.33.0 = Gauge32: 78
SNMPv2-SMI::enterprises.35450.34.34.0 = Gauge32: 88735915
```


## Using InfluxDB + Grafana

In order to easily monitor LeoFS, we provide LeoFS' input plugin of Telegraf[^1]. See more details on <a href ="https://github.com/influxdata/telegraf/tree/master/plugins/inputs/leofs/" target="_blank">its documentation</a>.
If you decide to use Telegraf[^1] as LeoFS' monitoring agent, you need to install InfluxDB[^2] and Grafana[^3].


![](/assets/leofs-monitoring-with-grafana.png)


## Using Prometheus + Grafana

We provide another way of LeoFS system monitoring which uses Prometheus [^4] and SNMP-Exporter [^5], and deliver `snmp.yml` of each component *(LeoGateway, LeoManager, and LeoStorage)* so that you can easily start.

* <a href="https://github.com/leo-project/leofs/blob/v1/apps/leo_gateway/priv/leo_gateway_snmp.yml" target="_blank">LeoGateway snmp.yml</a>
* <a href="https://github.com/leo-project/leofs/blob/v1/apps/leo_manager/priv/leo_manager_snmp.yml" target="_blank">LeoManager snmp.yml</a>
* <a href="https://github.com/leo-project/leofs/blob/v1/apps/leo_storage/priv/leo_storage_snmp.yml" target="_blank">LeoStorage snmp.yml</a>

A LeoFS' package includes each `snmp.yml` under each `etc` directory as below:

```bash
leo_gateway/etc/
└── leo_gateway_snmp.yml
leo_manager_0/etc/
└── leo_manager_snmp.yml
leo_manager_1/etc/
└── leo_manager_snmp.yml
leo_storage/etc/
└── leo_storage_snmp.yml
```

See more details on <a href ="https://github.com/prometheus/snmp_exporter" target="_blank"> SNMP-Exporter's documentation</a>.
If you decide to use Prometheus [^4], you need to install Grafana [^3] and SNMP-Exporter [^5] to LeoGateway nodes, LeoManager nodes, and LeoStorage nodes respectively.


## Related Links

- [Administrators / Settings / LeoManager Settings](../settings/leo_manager.md)
- [Administrators / Settings / LeoStorage Settings](../settings/leo_storage.md)
- [Administrators / Settings / LeoGateway Settings](../settings/leo_gateway.md)


[^1]: <a href="https://github.com/influxdata/telegraf" target="_blank">InfluxData / Telegraf - The plugin-driven server agent for collecting & reporting metrics</a>
[^2]: <a href="https://github.com/influxdata/influxdb">InfluxDB - An Open-Source Time Series Database</a>
[^3]: <a href="https://grafana.com/" target="_blank">Grafana - The open platform for analytics and monitoring</a>
[^4]: <a href="https://prometheus.io/" target="_blank">Prometheus - Monitoring system and timeseries database</a>
[^5]: <a href="https://github.com/prometheus/snmp_exporter" target="_blank">SNMP-Exporter - SNMP Exporter for Prometheus </a>
