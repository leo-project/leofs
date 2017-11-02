# FAQ: LeoFS Limits

<!-- toc -->

## Features

* LeoFS have covered almost major <a href="" target="_blank">AWS S3-API</a> but not all APIs.
* If you use <a href="http://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html" target="_blank">S3's Multi Part Upload API</a>, the size of a part of an object must be less than the size of a chunked object in LeoFS.
* When using the multi datacenter replication feature, we have supported up to 2 clusters with `LeoFS v1.2`, but we're going to support over 3 clusters replication with `LeoFS v2.0`.
* When you run recover-node while another recover-node is already working in-progress, objects that are not transmitted to the target node yet in the former recover-node are canceled and the latter recover-node will take place. To ensure that every recover-node completes its job, we'd recommend you to run recover-node one-by-one.

* Related Links:
    * <a href="http://docs.aws.amazon.com/AmazonS3/latest/API/Welcome.html" target="_blank">Amazon S3 REST API Introduction</a>
    * [LeoFS' ISSUE_177](https://github.com/leo-project/leofs/issues/177)
    * [LeoFS' ISSUE_338](https://github.com/leo-project/leofs/issues/338)
    * [LeoFS' ISSUE_880](https://github.com/leo-project/leofs/issues/880)


## Operations

* When you upgrade your LeoFS, you can NOT change the metadata storage as |KVS| - ``bitcask`` or ``leveldb`` can be used in LeoFS - used by LeoFS Storage. We recommend users to replace ``bitcask`` with ``leveldb`` by using |leofs_utils/tools/b2l|.

* Related Links:
    * [Configuration]()


## NFS Support

* NFS implemantation with `LeoFS v1.1` is a subset of <a href="https://tools.ietf.org/html/rfc1813" target="_blank">NFS v3</a>. ``Authentication``, and ``Owner/Permission`` management are NOT covered.
* The `ls` command may take too much time when the target directory have lots of child. We're planning to provide better performance with LeoFS v.2.0.
* If you use LeoFS with NFS, you should set the size of a chunked object in LeoFS to 1MB (1048576Bytes) - ``large_object.chunked_obj_len = 1048576`` *(leo_gateway.conf)*, otherwise the efficiency of disk utilization can be decreased.
* What are the requirements to run LeoFS with NFS?
	* LeoGateway (NFS Server):
		* We've supporeted the targets Debian 6, Ubuntu-Server 14.04 LTS or Higher and CentOS 6.5/7.0 as LeoFS does, but should work on most linux platforms. In addition, We've confirmed LeoFS with NFS works properly on the latest FreeBSD and SmartOS by using <a href="https://github.com/leo-project/leo_gateway/blob/develop/test/leo_nfs_integration_tests.sh" target="_blank">NFS Integration Test Tool</a>.

* Related Links:
    * [Configuration]()
