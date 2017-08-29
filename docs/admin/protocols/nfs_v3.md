# Interface: NFS v3
## Pre-requirement

We have checked the <a href="https://en.wikipedia.org/wiki/Network_File_System" target="_blank">NFSv3</a> server feature with CentOS 6.5, 7.x and Ubuntu Server 14.04, 16.04 LTS, but we did not strictly test other platforms, FreeBSD and SmartOS yet.

## Configuration

Update LeoGateway's protocol configuration to `nfs`, and configure NFS related configurations in your [LeoGateway configuration](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_gateway/priv/leo_gateway.conf)


```ini
## --------------------------------------------------------------------
## GATEWAY Protocol
## --------------------------------------------------------------------
## Gateway Protocol to use: [s3 | rest | embed | nfs]
protocol = nfs
.
.
.
## --------------------------------------------------------------------
## GATEWAY - NFS-related configurations
## --------------------------------------------------------------------
## Mountd's port number
nfs.mountd.port = 22050

## Mountd's the number of acceptors
nfs.mountd.acceptors = 128

## NFSd's port number
nfs.nfsd.port = 2049

## NFSd's the number of acceptors
nfs.nfsd.acceptors = 128
```

## Installation
### CentOS 6.x / 7.x

```bash
$ sudo yum install nfs-utils
```

### Ubnutu 14.04 / 16.04

```bash
$ sudo apt-get install nfs-common
```

## Start LeoFS as a NFS Server with other dependent programs

* Start a LeoFS storage system
	*  Ref: [Quick Installation and Setup](/installation/quick.md)
	*  Ref: [Building a LeoFS' cluster with Ansible](/installation/cluster.md)
* Start <a href="https://linux.die.net/man/8/rpcbind" target="_blank">rpcbind</a>

```bash
$ sudo service rpcbind start
```

* Create a bucket and a token for LeoFS' NFSv3 server with `leofs-adm gen-nfs-mnt-key <BUCKET> <ACCESS-KEY-ID> <CLIENT-IP_ADDRESS>`

```bash
$ ./leofs-adm add-bucket test 05236
OK

$ ./leofs-adm get-buckets
cluster id   | bucket   | owner       | permissions      | created at
-------------+----------+-------------+------------------+---------------------------
leofs_1      | test     | _test_leofs | Me(full_control) | 2014-07-31 10:20:42 +0900

$ ./leofs-adm gen-nfs-mnt-key test 05236 127.0.0.1
bb5034f0c740148a346ed663ca0cf5157efb439f
```

* Create a mount point and Mount

```bash
$ sudo mkdir /mnt/leofs

## for Linux - "sudo mount -t nfs -o nolock <host>:/<bucket>/<accesskey_id>/<token> <dir>"
$ sudo mount -t nfs -o nolock 127.0.0.1:/test/05236/bb5034f0c740148a346ed663ca0cf5157efb439f /mnt/leofs

## for FreeBSD - "sudo mount -t nfs -o nolockd <host>:/<bucket>/<accesskey_id>/<token> <dir>"
$ sudo mount -t nfs -o nolockd 127.0.0.1:/test/05236/bb5034f0c740148a346ed663ca0cf5157efb439f /mnt/leofs
```

* Now you can operate the bucket test in LeoFS as a filesystem via `/mnt/leofs`.


## Confirm that NFS works

* Create a file

```bash
$ touch /mnt/leofs/newfile
$ ls -al /mnt/leofs

drwxrwxrwx. 0 root root 4096 Jul 31 10:09 2014 .
drwxr-xr-x. 6 root root 4096 Jul 11 12:38 2014 ..
-rw-rw-rw-  0 root root    0 Jul 31 10:25 2014 newfile
```

* Modify a file

```bash
$ echo "hello world" > /mnt/leofs/newfile
$ cat /mnt/leofs/newfile

hello world
```

* Copy a file

```bash
$ cp /mnt/leofs/newfile /mnt/leofs/newfile.copy
$ ls -al /mnt/leofs

drwxrwxrwx  0 root root 4096 Jul 31 10:09 2014 .
drwxr-xr-x. 6 root root 4096 Jul 11 12:38 2014 ..
-rw-rw-rw-  0 root root   12 Jul 31 10:29 2014 newfile
-rw-rw-rw-  0 root root   12 Jul 31 10:31 2014 newfile.copy
```

* Check the file whether to store it into LeoFS with the `leofs-adm whereis` command

```bash
$ ./leofs-adm whereis test/newfile
-------+--------------------------+--------------------------------------+------------+--------------+----------------+----------------+----------------------------
 del?  |           node           |             ring address             |    size    |   checksum   |  # of chunks   |     clock      |             when
-------+--------------------------+--------------------------------------+------------+--------------+----------------+----------------+----------------------------
       | storage_0@127.0.0.1      | 22f3d93762d31abc5f5704f78edf1691     |        12B |   6f5902ac23 |              0 | 4ffe2d105f1f4  | 2014-07-31 10:29:01 +0900

$ ./leofs-adm whereis test/newfile.copy
-------+--------------------------+--------------------------------------+------------+--------------+----------------+----------------+----------------------------
 del?  |           node           |             ring address             |    size    |   checksum   |  # of chunks   |     clock      |             when
-------+--------------------------+--------------------------------------+------------+--------------+----------------+----------------+----------------------------
       | storage_0@127.0.0.1      | d02e1e52d93242d2dcdb98224421a1fb     |        12B |   6f5902ac23 |              0 | 4ffe2d20343a3  | 2014-07-31 10:31:17 +0900
```

* Diff files

```bash
$ diff /mnt/leofs/newfile /mnt/leofs/newfile.copy
```

* Remove a file

```bash
$ rm /mnt/leofs/newfile
$ ls -al /mnt/leofs

drwxrwxrwx  0 root root 4096 Jul 31 10:09 2014 .
drwxr-xr-x. 6 root root 4096 Jul 11 12:38 2014 ..
-rw-rw-rw-  0 root root   12 Jul 31 10:31 2014 newfile.copy
```

* Check the file whether to remove it into LeoFS with the `leofs-adm whereis` command

```bash
$ ./leofs-adm whereis test/newfile
-------+--------------------------+--------------------------------------+------------+--------------+----------------+----------------+----------------------------
 del?  |           node           |             ring address             |    size    |   checksum   |  # of chunks   |     clock      |             when
-------+--------------------------+--------------------------------------+------------+--------------+----------------+----------------+----------------------------
  *    | storage_0@127.0.0.1      | 22f3d93762d31abc5f5704f78edf1691     |         0B |   d41d8cd98f |              0 | 4ffe2e5d9cffe  | 2014-07-31 10:34:50 +0900
```

* Create a directory

```bash
$ mkdir -p /mnt/leofs/1/2/3
$ ls -alR /mnt/leofs/1

/mnt/leofs/1:
drwxrwxrwx 0 root root 4096 Jul 31 19:37 2014 .
drwxrwxrwx 0 root root 4096 Jul 31 10:09 2014 ..
drwxrwxrwx 0 root root 4096 Jul 31 10:37 2014 2

/mnt/leofs/1/2:
drwxrwxrwx 0 root root 4096 Jul 31 19:37 2014 .
drwxrwxrwx 0 root root 4096 Jul 31 19:37 2014 ..
drwxrwxrwx 0 root root 4096 Jul 31 10:37 2014 3

/mnt/leofs/1/2/3:
drwxrwxrwx 0 root root 4096 Jul 31 19:37 2014 .
drwxrwxrwx 0 root root 4096 Jul 31 19:37 2014 ..
```

* Remove files recursively

```bash
$ rm -rf /mnt/leofs/1/
$ ls -al /mnt/leofs

drwxrwxrwx  0 root root 4096 Jul 31 10:09 2014 .
drwxr-xr-x. 6 root root 4096 Jul 11 12:38 2014 ..
-rw-rw-rw-  0 root root   12 Jul 31 10:31 2014 leofs.copy
```

Other basic file OR directory operations also should work except controlling owners/permissions/symbolic links/special files.


## Limits
Since LeoFS NFS implementation is still the beta version, there are some limitations. The details are described at [LeoFS Limits](/faq/limits.md).


## Related Links

* [For Administrators / Settings / LeoGateway Settings](/admin/settings/leo_gateway.md)
