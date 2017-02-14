# What's New?

This page lists the new features and enhancements in the v1.3 series of LeoFS.

## [Release 1.3.2](https://github.com/leo-project/leofs/releases/tag/1.3.2)
### [AWS SDK for Python, Boto3](https://aws.amazon.com/sdk-for-python/) support

* According to a user's request, we have added Boto3 to LeoFS' client support.

#### Related Links

* <a href="https://github.com/leo-project/leofs_client_tests/tree/develop/boto3" target="_blank">Boto3 of LeoFS' client test</a>


## [Release 1.3.1](https://github.com/leo-project/leofs/releases/tag/1.3.1)
### User-defined metadata support

In order to provide optional information as a key-value pair into a metadata when you send a PUT request to store an object, you're able to set `use user-defined metadata` of the object.

#### Related Links

* <a href="http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html" target="_blank">S3 / Developer Guide / Working with Amazon S3 Objects/Object Key and Metadata</a>


## [Release 1.3.0](https://github.com/leo-project/leofs/releases/tag/1.3.0)
### AWS Signature v4 support

In order to cover latest AWS SDKs which includes <a href="https://aws.amazon.com/sdk-for-go/" target="_blank">Go</a>, <a href="https://aws.amazon.com/sdk-for-java/" target="_blank">Java</a> and others, we supported AWS Signature v4 with v1.3.0.

#### Related Links

* [AWS General Reference / Signing AWS API Requests / Signature Version 4 Signing Process
](http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html)
