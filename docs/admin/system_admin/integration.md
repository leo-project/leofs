# System Integration
## CDN Integration

### How To Integrate LeoFS With CDN

There is nothing special to do for integrating LeoFS with CDN[^1]. Since almost CDN service providers take care of a CacheControl Header received from an origin to determine how long a file should be cached on their edge servers, so if you want to modify TTL according to URLs, you can do it by using `http_custom_header.conf`.



### How To Use `http_custom_header.conf`

Append the following line to `leo_gateway.conf` which contains a LeoGateway's directory. Arrange the `http_custom_header.conf` into the path specified at `leo_gateway.conf`.

```ini
## HTTP custom header configuration file path
http.headers_config_file = ./etc/http_custom_header.conf
```

### How To Write `http_custom_header.conf`

The syntax is a subset of **Nginx[^2] configuration**. You can use location contexts to specify TTL and add any headers to the path.

```
location bucket/static {
    expires    12h;
    add_header Cache-Control public;
    add_header X-OriginalHeader OriginalValue;
}
```

In this case, assuming that a CDN service already has been enabled, and there is a file at `bucket/static/path_to_file`, if a user browses that file via the CDN. The CDN will receive a response from a LeoFS system with customized Http headers.

```
Cache-Control: public, max-age=43200;
X-OriginalHeader: OriginalValue;
```

### Use Cases

Specify TTL by the bucket.

```
location bucket1 {
    expires    1h;
    add_header Cache-Control public;
}
location bucket2 {
    expires    1d;
    add_header Cache-Control public;
}
location bucket3 {
    expires    1h30m;
    add_header Cache-Control private;
}
```

### Appendix
#### Syntax for the expire field

LeoFS supports a part of measurement units which can be used in Nginx configuration. Following time intervals can be specified.

```
s: seconds
m: minutes
h: hours
d: days
```

#### List of verified CDN services

LeoFS Team tested the following CDN services with LeoFS. We recognize that other CDN services also should work.

- <a href="https://aws.amazon.com/cloudfront/" target="_blank">Amazon CloudFront – Content Delivery Network (CDN)</a>


## Related Links

- [For Administrators / Settings / LeoGateway Settings](/admin/settings/leo_gateway.md)



[^1]: <a href="https://en.wikipedia.org/wiki/Content_delivery_network" target="_blank">CDN, Content delivery network</a>

[^2]: <a href="https://nginx.org/" target="_blank">Nginx, An HTTP and reverse proxy server</a>