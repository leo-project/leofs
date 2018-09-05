# Production Checklist: LeoFS

<!-- toc -->

## What version should we use?
Use the latest stable one. With the version <= v1.4.0, LeoFS had some serious issues that may cause data-lost, so that use at least >= v1.4.1. Or in case you need to keep running LeoFS with older one for some reason, make sure that you avoid those issues with the below check list.

## Check list to avoid serious issues for those who have to keep running some older version of LeoFS

- [large_object.reading_chunked_obj_len](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_gateway/priv/leo_gateway.conf#L126) <= [large_object.chunked_obj_len](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_gateway/priv/leo_gateway.conf#L117) in `leo_gateway.conf`. This setting prevents LeoFS from suffering [LeoFS Issue#531](https://github.com/leo-project/leofs/issues/531).

- If you are supposed to use multipart upload feature then set the part size to something less than 10MB. This setting prevents LeoFS from suffering [LEOFS_Issue#1017](https://github.com/leo-project/leofs/issues/1017).

- Never use PUT Object acl request[^1]. Not using PUT Object acl prevents LeoFS from suffering [LEOFS_Issue#1021](https://github.com/leo-project/leofs/issues/1021).

[^1]: <a href="https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectPUTacl.html" target="_blank">PUT Object acl</a>

