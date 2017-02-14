# Production Checklist: LeoFS

<!-- toc -->

## What version should we use?
Use the latest stable one. With the version <= v1.3.0, LeoFS had a serious issue that may cause data-lost, so that use at least >= v1.3.1. Or in case you need to keep running LeoFS with older one for some reason, make sure that [large_object.reading_chunked_obj_len](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_gateway/priv/leo_gateway.conf#L126) <= [large_object.chunked_obj_len](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_gateway/priv/leo_gateway.conf#L117) in `leo_gateway.conf`. This setting prevent LeoFS from suffering [LeoFS Issue#531](https://github.com/leo-project/leofs/issues/531).

The last part of a large object can be broken with [reading_chunked_obj_len](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_gateway/priv/leo_gateway.conf#L126) > [chunked_obj_len](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_gateway/priv/leo_gateway.conf#L117) in `leo_gateway.conf`.