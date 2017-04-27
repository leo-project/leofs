# Hardware Requirements
## Minimum Requirements

|         | CPU | Memory | Disk   |
|---------|-----|--------|--------|
| Manager |  1  | 512 MB | 20 GB  |
| Gateway |  2  | 1 GB   | 20 GB  |
| Storage |  2  | 1 GB   | 100 GB |


## Hardware Recommendation

1. **Workload on Manager is low**, it does not consume many resources
2. **Better CPU allows LeoGateway** to process more operations (OPS, *Operation Per Second*)
3. LeoGateway utilizes **memory and disk as cache**, adding those resources can reduce the workload to LeoStorage
4. **SSD** on a LeoStoage node significantly improves small object read performance
5. **10Gbps network** is recommended


## Reference Platform

| Hardware | Detail                    |
|----------|---------------------------|
| CPU      | 16C32T (Intel E5-2630 v3) |
| Memory   | 32 GB                     |
| Network  | 10 GbE                    |
| Disk     | SSD (Crucial BX100)       |


### Performance

| Data Set           | Read      | Write     | Resource Usage |
|--------------------|-----------|-----------|----------------|
| Image (32KB)       | 20,000 OPS| 20,000 OPS| High CPU Usage |
| Small Mixed (<2MB) | 1,200 OPS | 1,500 OPS | High Disk I/O  |


## Related Links

- [For Administrators / Setup / Planning for Production](planning_for_production.md)
- <a href="https://github.com/leo-project/notes/tree/master/leofs/benchmark/leofs" target="_blank">leo-project/notes - LeoFS Benchmark Report</a>