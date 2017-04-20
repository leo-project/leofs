# LeoFS Manual
## Why LeoFS?

LeoFS is a highly available, distributed, eventually consistent object/blob store. If you are searching a storage system that is able to store huge amount and various kind of raw data in its native format, LeoFS is suitable for that.

LeoFS supports the following features:

* Multi protocols support
	* RESTful Interface
	* Amazon S3-API[^1]
		* A S3 compatible storage system
		* Switch to LeoFS to decrease your cost from more expensive public-cloud solutions.
	* NFS v3[^2]
* Built-in object cache feature
* Multi data center/cluster replication
* User and bucket management
* Data lake solution
* Cloud solution integration

To try LeoFS, see our [Getting Started guides](installation/quick.md), and to learn more about LeoFS, see our [Architecture section](architecture/README.md).

## Goals

We have been aiming to achieve three things:

* High reliability and availability
  * Operating ratios is 99.9999999%, nine nines
  * Keeps maintaining high data and system availability while some node downed in a LeoFS' cluster
* High scalability
  * Builds a huge capacity cluster at low cost
  * Supports a multi data center/cluster replication
* High cost performance ratio
  * High performance with minimum resources
  * Provides easy management and easy operation to users


## Resources

- <a href="http://leo-project.net/leofs/" target="_blank">LeoFS Website</a>
- <a href="https://github.com/leo-project/leofs" target="_blank">LeoFS Repository on GitHub </a>
- LeoFS Download Page
	- <a href="http://leo-project.net/leofs/download.html" target="_blank">LeoFS Package for CentOS and Ubuntu</a>
	- <a href="http://www.freshports.org/databases/leofs" target="_blank">LeoFS Package for FreeBSD </a>


[^1]: <a href="http://docs.aws.amazon.com/AmazonS3/latest/API/Welcome.html" target="_blank">Amazon S3 API</a>
[^2]: <a href="https://de.wikipedia.org/wiki/Network_File_System" target="_blank">Wikipedia: Network File System</a>