# Hardware Requirements
## Minimum Requirements

|         | CPU | Memory | Disk   |
|---------|-----|--------|--------|
| Manager |  1  | 512 MB | 20 GB  |
| Gateway |  2  | 1 GB   | 20 GB  |
| Storage |  2  | 1 GB   | 100 GB |

## Hardware Recommendation
1. Workload on Manager is low, it does not consume much resource
2. Better CPU allows LeoFS to process more operations (OPS)
3. Gateway utilizes memory and disk as cache, adding those resources can reduce the workload to Storage
4. SSD significantly improves small object read performance
5. 10Gbps network is recommended

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
| Image (32KB)       | 20000 OPS | 20000 OPS | High CPU Usage |
| Small Mixed (<2MB) | 1200 OPS  | 1500 OPS  | High Disk I/O  |

## Related Links
- [Benchmark Report](https://github.com/leo-project/notes/tree/master/leofs/benchmark/leofs)
