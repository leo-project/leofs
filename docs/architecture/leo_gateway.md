# LeoGateway's Architecture

LeoGateway consists of the fast HTTP server which is <a href="https://github.com/ninenines/cowboy" target="_blank">Cowboy</a>, the multi-protocols handler and the objecct cache. It provides the RESTful API and <a href="http://docs.aws.amazon.com/AmazonS3/latest/API/Welcome.html" target="_blank">S3-API</a>. You’re able to easily access LeoFS with S3 clients which includes <a href="http://s3tools.org/s3cmd" target="_blank">s3cmd</a>, <a href="http://www.s3-client.com/" target="_blank">DragonDisk</a> and AWS SDKs - <a href="https://aws.amazon.com/sdk-for-java/" target="_blank">Java</a>, <a href="https://aws.amazon.com/sdk-for-ruby/" target="_blank">Ruby</a>, <a href="https://docs.aws.amazon.com/sdk-for-go/api/service/s3/" target="_blank">Go</a>, <a href="https://aws.amazon.com/sdk-for-python/" target="_balnk">Python (Boto3)</a> and others.


![](../assets/leofs-architecture.002.jpg)

A client requests an object or a bucket operation to a node of LeoGateway, then it requests the message of an operation to a node of LeoStorage.

A destination storage node is decided by RING (routing table), which is generated and distributed at nodes of LeoManager.

LeoGateway also provides built-in support for the object cache to realize keeping high performance and reduction of traffic between LeoGateway's nodes and LeoStorage's nodes.
