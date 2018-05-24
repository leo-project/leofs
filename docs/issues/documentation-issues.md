# Documentation Issues

- Status: in-progress
- Start Date: 2017-01-11
- Reference: <a href="https://groups.google.com/forum/#!topic/leoproject_leofs/QsxY2wNWhYg" target="_blank">LeoFS ML: Ideas about improving documentation</a>

## Administration

1. Regular operations
  - What is best practice for regular (e.g. once per month or every few months) "scrub" of data? To force each node verify that it contains all objects it's supposed to contain and that all of actual data passes checksum check? "diagnose-start" on each node? Or there is some better / recommended way.
    - [ ] **Unfortunately there is no official tool to scrub (some product call it anti-entropy) all data efficiently. actually there was in the past at <a href="https://github.com/leo-project/leofs_utils" target="_blank">LeoFS Utils</a> but disappeared for some reason. anyway we will provide|publish an official tool to scrub safely and efficiently. One solution for scrub is to call leofs-adm recover-consistency on every node you have.**

  - Also, by default option "object_storage.is_strict_check = false" is set. Does it mean that checksum check for data is not performed or it's about something else?
    - [ ] **Yes that means checksum is checked or not.**

  - LeoManager maintenance commands like backup-mnesia / restore-mnesia and dump-ring. Are they strictly for debug purposes or it's good practice to do these mnesia backups and ring dumps regularly and store result on backup server or something?
    - [ ] **Yes it's good practice to backup mnesia regularly through those tools**

## Installation

1. [Building a cluster](https://leo-project.net/leofs/docs/getting_started/getting_started_2.html)
  - setting nodename is required, but doesn't talk about setting manager.partner. Is it required or not? If not, what happens when it's not set? If yes, then master and slave nodes *do* know about each other? Can't they check each others' status then?
    - [ ] **`manager.partner` is required so we will fix.**

2. Installation Guide
  - some reasons should be listed in documentation why installing from source is "recommended" on production nodes and packaged version is only for development / testing.
    - [ ] **Document to be fixed by rather recommend the package one.**

3. XFS-related
  - "highly recommends" using XFS. I'd like to know if this recommendation is still valid. Do benchmarks still show any performance benefit of XFS over ext4 on modern kernel (e.g. one from EL7)?
    - [ ] **Will do benchmarks to confirm XFS still shows any performance benefit**

## Common
1. Configuration guide - fundamentals
  - "Erlang VM related properties". Are there any reasons to touch these settings for certain configurations? Maybe some hint - like, "for this kind of load you might want to tweak this parameter" - would be nice.
    - [ ] **Will document as the other Erlang products do**
  - "Configuration of eager check I/O scheduling for Erlang’s VM" - if this is recommended, how come official packages that come with erlang 17.5 have this option disabled?
    - [x] **Official packages should be fixed with having this option enabled**
  - Are there plans to update it to 19 now, by the way, now that 19 is supported?
    - [x] **Yes we will. we at first will ship official packages with 18.x after finishing tests sufficiently**

## LeoManager

1. LeoManager's Master / Slave management
  - There is no clear definition on how the system works if slave node is not present. Since you can't see slave node in "leofs-adm status" command, it's pretty unobvious what actually changes when you launch it. Nothing appears in status commands, nothing appears in log files on management node.
    - [ ] **Will document how it works while slave node is not present.**
    - [ ] **Will provide information on how to check on health of slave node except SNMP (more simpler way)**

  - by default, leofs-adm isn't working on slave node! It took me quite a time to realize (by looking at leo-center) that you actually *can* connect to slave node with leofs-adm, only with default config you have to specify different port.
    - [ ] **Will document and preferably generate a leofs-adm for slave when do `make release`**

  - However, when you specify port, leofs-adm output changes to JSON style and there is no obvious way to get same output as you get from the master node.. confusing. This should be mentioned in documentation, that leofs-adm works with slave node (at first I thought it just won't work at all), but default configuration sets different management port for slave.
    - [ ] **Will document that there are two ports in each manager, the one is formatted in text for console, the another one is formatted in json for api (machine readalbe one)**

2. Configuration guide - fundamentals
  - The consistency level is configured in this file. It should not be modified while the system is running." I believe this should be rephrased somewhat.
    - [ ] **Yes. should be rephrased by "consistency.num_of_replicas should not be modified while the system  is running.""**
  - What would happen if I'm to actually modify it while system is running? and Which parts of system I'm supposed to restart if I'm to change it in this file? Also it kind of gives impression that it's not possible to modify consistency level on running system, but there is "leofs-adm update-consistency-level" command for that!
    - [ ] **Will document that what would happen when modifying consistency.num_of_replicas while system is running and which parts of system should be restarted to reflect changes**
    - [ ] **Will document that if you want changes to be temporal, just do update-consistency-level**
    - [ ] **Will document that if you want changes to be parmanent, edit the section of consistency.\* in leo_manager.conf and also do update-consistency-level that distributes the change to all of the members without forcing them to restart.**
  - "consistency.rack_aware_replicas     # of rack-aware replicas" - there is no information about what it does
    - [ ] **Will document the details**

## LeoStorage

1. Configuration guide - LeoStorage
  - "You cannot modify the following LeoStorage configurations after launched a system(LeoFS) because the algorithm of retrieving object depends on their configurations, that means Leo Storage cannot handle objects." but what if I *do* want to modify these parameters? This is rather practical problem, say, I need to add few new paths to obj_containers.path. Right now there is no explanation how to accomplish it correctly, only this warning which tells you not to do this.
    - [ ] **Short version: as described at the current docs, there is no easy way to accomplish modifying what you want**
    - [ ] **Long version a specific answer to adding new paths to obj_containers.path: unfortunately just adding new paths don't work instead you have to do the following. (you have to do the same procedure if you want to modify obj_containers.num_of_containers**
        1. **Suspend and stop the target server and backup -r under avs dir**
        2. **Edit the obj_containers.path in leo_storage.conf**
        3. **Remove -rf under avs dir**
        4. **Restart and resume the server**
        5. **Do leofs-adm recover-node**

2. Architecture - LeoStorage
  - Lots of topics where extra documentation would help are already listed in <a href="https://github.com/leo-project/leofs/issues/532" target="_blank">LeoFS Issue#532</a>
    - [ ] **Will do #532 ASAP**
  - I'd like to add that information about how and when storage nodes do fsync() for data files is missing. Is it done after writing every object? Or on timer? Is it configurable?
    - [x] **Now no explicit call file:fsync because even if we call fsync some blocks can be still on the volatile place (the disk controller can choose not to obey that which unfortunately many disks choose to) but we will consider to take it another look as at least it increase a possibility to be placed at a real permanent one**

3. Best Practices
  - how vital is queue in work/queue directory? I mean, obviously data on storage node itself won't be lost if this queue is lost or damaged; but under load, there is information related to replication and such in these messages, right?
    - [ ] **Yes a message in queue may have information related to replication**
  - If they are suddenly lost on some node, are any extra steps needed to recover anything that might've been lost?
    - [x] **Yes. you need some extra steps to recover any files that don't satisfy the redundancy requirement specified at consistency.num_of_replicas in leo_manager.conf. we'd like to answer what the extra step actually is at your another question "scrub of data"**

  - Regarding all kinds of metadata (mnesia on management nodes, leveldb queues on all nodes, leveldb metadata on storage nodes) it would be nice to have some information about their possible size and I/O required for that. I mean, it might be obvious how to estimate size of main storage and possible to make guesses about I/O load on AVS files, but exactly how much space various kinds of metadata can consume on loaded production systems? And whether it might require lots of extra I/O, e.g. HDD for main storage + SSD for metadata or something. If most users shouldn't worry about this - it's fine too, but a brief note like "metadata does not require lots of I/O and queues shouldn't consume more than 100 MB, metadata for AVS files - estimate is <x> bytes per stored record" would be great to have.
    - [ ] **Great point especially for loaded production sysmtes. we can't provide actual numbers right now but can say that metadata using leveldb can consume your I/O load more than expected due to its compaction activity. part of problems we encountered has been fixed at <a href="https://github.com/leo-project/leofs/issues/555" target="_blank">LeoFS Issue#555</a> and also we are now tackling at <a href="https://github.com/leo-project/leofs/issues/503" target="_blank">LeoFS Issue#503</a>. We will publish the actual number (extra space, # of size, I/O load etc) ASAP**

  - Of course, whether I/O on metadata and queues size might become an issue during rebalance and recover-node operations should be mentioned as well.
    - [ ] **Will provide the some reference number to estimate the approximate load**

  - If there is single obj_containers.path per huge drive (e.g. 8 TB), then with default setting of 8 containers it will create up to 1 TB AVS files (well, not really but..). Since file has to be rewritten during compaction, it would really suck. I think some recommendations about single file size / number of containers should be provided, because it's really hard to guess which size to prefer here and if there are downsides of having lots of containers and smaller files. Even something vague like "we recommend to pick number of containers so that single file size will be in 20-200 GB range" would help - thus users would know that they should pick the number of containers in 40-400 range for said 8 TB drives. There is no need to guide the users by hand, just brief information of advantages and disadvantages of having huge files and few containers vs. smaller files and lots of containers. Like, maybe having lots of containers would require extra memory and resources, and having each container too huge would cause problems with leveldb metadata storage.
    - [ ] **There is a [FAQ](http://leo-project.net/leofs/docs/faq/faq_3.html#how-do-i-set-a-number-of-containers-at-leofs-storage-configuration) but have not covered all topics you want to know so we will update this FAQ**

  - num_of_vnodes - should this parameter ever be changed? (e.g. when having lots of storage nodes or lots of containers per node).
    - [ ] **There is a logic that calculate an appropriate `num_of_vnodes` based on some facts like # of maximum storage nodes, # of maximum objects and so on. @yosukehara will answer later.**

  - How data is distributed between AVS files - equal amount per file? In other words, if, say, I have nodes - 8 drives, 1 TB each but at some point started to add nodes - 8 drivers, 2 TB each, am I supposed to double amount of containers on these new nodes in order to utilize these 2 TB of data (I understand it would be bad from I/O point of view, of course)? Or it's more complicated than that?
    - [ ] **Equal amount per file. Setting the appropriate value to `num_of_vnodes` on the corresponding node should solve the problem - deviation of disk space by node**

## LeoGateway

1. Configuration guide - LeoGateway
  - there is a problem which made me stumble for quite some time. I created a copy of leo_gateway directory for testing NFS and S3 gateways along with REST. I configured them to different ports, including SNMP ports and such but for some reason I just couldn't launch REST and S3 gateways together. NFS + REST - fine, NFS + S3 - fine, but REST + S3 - didn't work. They both launched fine, but the one launched later simply refused to work (also, it had status of "stopped" in leofs-adm). Turned out, I have copied leo_gateway directory to leo_s3_gateway after launching it once.. and there was something in work/queue/membership that made these two gateways "the same" - despite having different node names. After removing that directory from one of the nodes and re-launching it it worked fine.
    - [ ] **Will consider to add this knowledge to the FAQ section.**
