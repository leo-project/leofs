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
