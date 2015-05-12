## leofs-docker
Docker container image for LeoFS.

### Installation
```
$ docker pull leofs/leofs:latest
```
### Startup LeoFS
```
$ docker run -d leofs/leofs:latest
```
### Confirm container ID
```
$ docker ps -a -q
{Container ID}
```
### Check startup log
```
$ docker logs {Container ID}
* Starting OpenBSD Secure Shell server sshd
   ...done.
tput: No value for $TERM and no -T specified
tput: No value for $TERM and no -T specified
Generating RING...
Generated RING
OK 100% - storage_0@127.0.0.1
OK
LeoFS is ready!!
```
### Confirm IP Address
```
$ docker inspect {Container ID} | grep IPAddress
    "IPAddress": "172.17.0.XXX",
```
### Login by SSH
```
ID: leofs Password: leofs

$ ssh leofs@172.17.0.XXX
```
### leofs-adm command
```
~$ leofs-adm status
 [System Confiuration]
-----------------------------------+----------
 Item                              | Value
-----------------------------------+----------
 Basic/Consistency level
-----------------------------------+----------
                    system version | 1.2.8
                        cluster Id | leofs_1
                             DC Id | dc_1
                    Total replicas | 1
          number of successes of R | 1
          number of successes of W | 1
          number of successes of D | 1
 number of rack-awareness replicas | 0
                         ring size | 2^128
-----------------------------------+----------
 Multi DC replication settings
-----------------------------------+----------
        max number of joinable DCs | 2
           number of replicas a DC | 1
-----------------------------------+----------
 Manager RING hash
-----------------------------------+----------
                 current ring-hash | 433fe365
                previous ring-hash | 433fe365
-----------------------------------+----------

 [State of Node(s)]
-------+--------------------------+--------------+----------------+----------------+----------------------------
 type  |           node           |    state     |  current ring  |   prev ring    |          updated at
-------+--------------------------+--------------+----------------+----------------+----------------------------
  S    | storage_0@127.0.0.1      | running      | 433fe365       | 433fe365       | 2015-04-01 00:00:00 +0000
  G    | gateway_0@127.0.0.1      | running      | 433fe365       | 433fe365       | 2015-04-01 00:00:00 +0000
-------+--------------------------+--------------+----------------+----------------+----------------------------
```

### Change target version
You can change target version.
```
Dockerfile:Line 3:
ENV LEOFS_VER={TARGET VERSION} PATH=/usr/local/erlang/17.4/bin:$PATH
ex)
ENV LEOFS_VER=1.2.8 PATH=/usr/local/erlang/17.4/bin:$PATH
```

When you want to know more about details, please visit document pages.

### Links
- [LeoFS Website](http://leo-project.net/leofs/)
- [LeoFS Document](http://leo-project.net/leofs/docs/index.html)
- [LeoFS on GitHub](https://github.com/leo-project/leofs)
