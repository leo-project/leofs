# Architecture

What we’re really focused on is `high availability`, `high scalability` and `high cost performance ratio` because unstructured data which have been exponentially increasing day by day, and we needed to more efficiently manager objects to find values from tons of raw data.

We really succeeded in designing and implementing LeoFS as simple as possible. LeoFS consists of three components, [LeoManager](leo_manager.md), [LeoStorage](./leo_storage.md) and [LeoGateway](./leo_gateway.md). The role of each component is clearly defined.

![](../assets/leofs-architecture.001.jpg)

What we also carefully desined LeoFS is 3 things:

* To keep running LeoFS without SPOF
* To keep maintaining high performance regardless of the kind of data and amount data
* To provide easy administration by the LeoFS' CLI, [leofs-adm](https://github.com/leo-project/leofs/blob/master/leofs-adm)