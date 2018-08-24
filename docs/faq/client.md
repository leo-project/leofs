# FAQ: LeoFS Clients

<!-- toc -->

## Name Resolution

* There are two ways to access LeoGateway from LeoFS' Clients.
    * Path Style Access
    * Sub-Domain Style Access
* If a LeoFS Client you use adopts the sub-domain style access, then the client need to resolve **bucket.endpoint** domain into the IP address of LeoGateway.

* Related Links:
    * [A Comment on LeoFS' Issue #748, Connecting to bucket causes badarg error](https://github.com/leo-project/leofs/issues/748#issuecomment-306391378)
    * [For Administrators / System Operations / S3 / Endpoint](../admin/system_operations/s3.md#endpoint)

## User Defined Metadata

Although we support User Defined Metadata according to the original S3 spec, we have reserved every key which prefix is "x-amz-meta-leofs-" as system reserved ones. So please be careful not to use User Defined Metadata with the key prefixed "x-amz-meta-leofs-".

* Related Links:
    * [User Defined Metadata](https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-metadata)

## Uploading Objects

See [Administrators / Settings / LeoGateway Settings - Uploading Objects](/admin/settings/leo_gateway.md)

## Can't read files stored in LeoFS using s3fs

s3fs can make a part request in a different size on each part as described below.

```
[PUT]   test    test/test.parquet       1       5596007 2018-08-01 14:28:13.217763 +0900        1533101293217754 200     49
[PUT]   test    test/test.parquet       2       5246231 2018-08-01 14:28:13.378359 +0900        1533101293378348 200     34
[PUT]   test    test/test.parquet       3       553677  2018-08-01 14:28:13.400117 +0900        1533101293400082 200     7
```

(The fifth column denotes the size of a body LeoGateway receives.)

However LeoGateway expects the part size to be same among all parts except the last one so that LeoGateway may fail to calculate the position where to start reading when handling a GET with Range Request. As a result, LeoGateway may respond wrong data to clients and that's the case.

### Workaround

There is a init param "default_block_size" which allow library users to specify the threshold over which an object will be sent through multipart upload so setting a enough large value to the param make multipart uploads never happen. The below code is an example to avoid multipart uploads with s3fs.

```python
boto3_dict = {'endpoint_url':'http://localhost:8080'}
fs = s3fs.S3FileSystem(key= ACCESS_KEY, secret = SECRET_ACCESS_KEY,
                       default_block_size = 104857600,
                       client_kwargs=boto3_dict)
```

With the above code, you can read/write files which size are less than 100MB with s3fs.

* Related Links:
    * [s3fs](https://github.com/dask/s3fs)
