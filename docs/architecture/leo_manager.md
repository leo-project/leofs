# LeoManager's Architecture

LeoManager generates and manages a routing table, which is called RING and is based on <a href="https://en.wikipedia.org/wiki/Consistent_hashing" target="_blank">consistent hashing</a>.

LeoManager always monitors status and RING's consistency of nodes of LeoStorage and LeoGateway to keep running LeoFS and keep a RING's consistency, and it also distributes a RING to nodes of LeoStorage and LeoGateway.

![](../assets/leofs-architecture.007.jpg)

In addition, LeoManager provides [LeoFS administration commands]() to be able to easily operate LeoFS. The administration commands already cover entire LeoFS features.