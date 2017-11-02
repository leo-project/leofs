# Running LeoFS gateways behind load balancer

## Example configuration

Using load balancer (LB) in front of at least two LeoGateways that are operating in S3 or REST mode is always a good idea for production and staging systems; besides distributing the load between multiple gateways, it provides ability to restart and upgrade gateway servers without affecting service. It's likely that any LB that works in HTTP mode can be used in front of LeoGateways (TCP and IP-level LBs are a bad choice for this). Here is example configuration for HAProxy[^1] (tested on version 1.6) in front of 4 backends (LeoGateways).
```
global
    log 127.0.0.1     local0
    log 127.0.0.1     local1 notice
    user haproxy
    group haproxy
    maxconn 90000
    spread-checks 5

defaults
    log     global
    option  dontlognull
    option  redispatch
    option  allbackups
    no option  httpclose
    retries 3
    maxconn 90000
    timeout connect 5000
    timeout check   3000
    timeout client  30000
    timeout server  35000

frontend leofs
    bind    192.168.130.66:8080
    mode    http
    option  httplog
    capture request header Host len 64
    use_backend leofs_backend

backend leofs_backend
    mode    http
    balance roundrobin
    option  httpchk GET /_leofs_adm/ping HTTP/1.1\r\nHost:\ s3.amazonaws.com
    timeout http-keep-alive 4500

    server    gw0 gw0.lan:8081 weight 1 maxconn 10000 check
    server    gw1 gw1.lan:8081 weight 1 maxconn 10000 check
    server    gw2 gw2.lan:8081 weight 1 maxconn 10000 check
    server    gw3 gw3.lan:8081 weight 1 maxconn 10000 check
```

## Health Check URL

One of the important options here is `option httpchk`, which makes LB check if gateway is working and can connect to storage servers. Problematic gateways (which refuse connection, reply with an error or don't provide reply fast enough, which is controlled by `timeout check` parameter here) can be automatically disabled / re-enabled this way. The URL for health check is always **/_leofs_adm/ping**, regardless of gateway mode (S3 or REST). For S3 mode, "Host:" header is required as well (any S3 endpoint can be used there). Similar feature should be activated in configuration of any LB.

!!! note "Note: Health Check in REST mode"
    You can't get an object stored at **/_leofs_adm/ping** as it's shadowed by the Health Check URL so be careful not to put any object at **/_leofs_adm/ping** in REST mode.


## Things to check for production systems

Generally, it's best to use latest version of HAProxy (or, at very least, 1.6). This example is suited (and probably doesn't require much tweaking) for people who just need LB for extra reliability. However, please note that running a high load production system would require tweaking of various timeouts, LB / backend and LB / client connection parameters. Depending on clients and type of load, there is no single perfect solution; it requires tweaking, experimentation and performance measurements (most LBs have internal statistics which can help here), besides, often OS-level tweaking is required as well. Here are some important points both for tweaking this configuration to handle high load and for configuring other load balancers.

**Open files limit**: each connection uses an open file (socket), and operating system limits amount of open files for processes. Generally, for users of official Linux packages that are running LeoFS v1.3.8 and higher these limits should be increased automatically. For other cases, please consult documentation for your OS / distro on how to properly increase file limits for processes; it's a good idea to set this limit to high value like 65535 for LeoGateway (usually, default is 1024). Open files limit should be raised for LB as well, it should be bigger than maximum allowed amount of connections to all backend servers plus maximum amount of client connections; in case of HAProxy, it must be set higher than number in global `maxconn` option (e.g. 31000 for this example with `maxconn 30000`).

**Request timeouts**: LB can impose a time limit on how fast backend server must give a reply (time before it starts sending HTTP headers). In case of HAProxy, it's `timeout server` option: when backend takes more time to reply than the time specified there, HAProxy will send "504 Gateway Timeout" to client instead. However, since LeoGateway has its own timeout logic, aborting request like that is a bad idea, since gateway was still busy working on that request. Either LB timeouts or gateway timeouts (or both) should be tweaked and LB server-side timeout should be set **bigger** than timeouts in leo_gateway.conf (`timeout.get` and `timeout.ls`, which are 30 seconds by default). This way, gateway will give its own (503) reply to client when it can't process request due to high load and LB will generate "504 Gateway Timeout" only in special cases like network problem between LB and backend. Since timeouts in leo_gateway.conf are set for internal operation between LeoGateway and LeoStorage, the overall delay between request to gateway and reply might be bigger than that, so timeout in LB configuration should be set appropriately. Also, when requesting a large object, gateway can insert delays between chunks of data up to `timeout.get` value. Some LBs need to be tweaked to handle these delays without closing the connection (in case of HAProxy, it's `timeout tunnel` option), though generally it shouldn't be a problem.

Example:
```
# timeouts in leo_gateway.conf set to 30 seconds
timeout.get = 30000
timeout.ls = 30000

# timeout for headers in haproxy.cfg set to higher value - 35 seconds
# mid-stream inactivity timeout (both directions) set to 10 minutes
backend
    timeout server 35000
    timeout tunnel 10m
```

**HTTP connection reuse**: many LBs offer a feature of reusing the same connections to backends to handle requests from various clients. Generally this mode isn't default because it doesn't work with all types of backends. However, it works with LeoGateway since it operates in stateless (REST) mode (this includes operating in S3 protocol mode which follows REST as well). Under some conditions, like requests from very high amount of clients, this option can significantly decrease latency and reduce load on LB and gateway, since it allows to use much smaller amount of connections between LB and backend compared to amount of connections between LB and clients, and save time / CPU on re-establishing TCP connections to backend. In HAProxy, this is controlled by `http-reuse` directive, e.g. `http-reuse always`.

**Long-lived keep-alive tunnels**: alternative to the above, LBs can use one-on-one connection mappings between clients and backends. Each connection from client creates new connection to backend, which is closed when client closes the connection, so they behave like tunnels from client to backend. Under some conditions (clients use HTTP keep-alive, low amount of clients, long-lived connections with high rate of requests) this might be a better option compared to connection reuse. Modern versions of HAProxy use this mode by default, but older versions (1.5 or earlier) and other LBs might behave differently. With this style of connections, number of connections from clients that LB can handle is limited by maximum allowed amount of connections to all active backends. This mode shouldn't be confused with keep-alive connections only between clients and LB.

Activating either of these features (reusing connection to backends or tunnels with keep-alive to both client and backend) can significantly reduce load and increase performance compared to establishing connection between LB and backend for every request. However, the downside is that memory requirements for LB are increased and some extra tweaking might be needed. The most important tweak is keep-alive timeout between LB and backend. LeoGateway has timeout parameters (`http.timeout_for_header` and `http.timeout_for_body`), first of which is set to 5s by default. It means that keep-alive connection without any traffic in it for 5 seconds will be closed by LeoGateway. It's a good idea to increase this parameter when operating in the above modes. Regardless whether it's increased or not, the similar parameter on LB side should be set to **lower** value than timeout on LeoGateway. In HAProxy, this is controlled by `timeout http-keep-alive` in the backend section. This is especially important when HTTP connections are reused, because it avoids the problem when LB tries to send data into existing connection at the same time as LeoGateway closes that connection. Ensuring that LeoGateway keeps the inactive connection open a bit longer than the time after which LB will close connection from its side solves this problem.

Example:
```
# keep-alive timeout between requests in leo_gateway.conf set to 30 seconds
http.timeout_for_header = 30000

# keep-alive timeout in haproxy.cfg set to smaller value - 29 seconds
backend
    timeout http-keep-alive 29000
```


**SSL/TLS termination**: obviously, for LB to work with LeoGateway in HTTP protocol mode, SSL has to be terminated on LB itself. Usually (e.g. in case of HAProxy in multi-process mode) it will provide much higher SSL performance compared to serving SSL from LeoGateway directly, so LeoGateway should always be configured to use HTTP when operating behind LB. In cases where secure traffic between LB and LeoGateway is a must, either LB should be configured to decrypt/re-encrypt SSL traffic (so it can still operate in HTTP and not in TCP tunneling mode), or some other layer of encryption, like VPN should be used.

**High availability**: a single load balancer becomes a point of failure (PoF) itself. For highly available configurations, at least two LBs are needed, configured for failover (e.g. using floating IP with Corosync / Pacemaker[^2]). Alternatively, IP-level LB (e.g. Linux Virtual Server[^3]) can be set up in front of HTTP-level LBs. IP-level balancing solutions provide higher availability compared to failover and can distribute load between multiple HTTP-level LBs, but require dedicated hardware.


## Related Links

- [For Administrators / Settings / LeoGateway Settings](/admin/settings/leo_gateway.md)



[^1]: <a href="http://cbonte.github.io/haproxy-dconv/1.6/configuration.html" target="_blank">HAProxy version 1.6 - Configuration Manual</a>

[^2]: <a href="https://www.clusterlabs.org/" target="_blank">High Availability Cluster Stack</a>

[^3]: <a href="http://www.linuxvirtualserver.org/" target="_blank">The Linux Virtual Server Project</a>
