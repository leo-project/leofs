# LeoFS Gateway - Internal Network Authentication Bypass

## Status: Implemented

**Last Updated**: 2025-12-20

## Overview

This document describes the design for implementing an internal network authentication bypass feature in LeoFS Gateway. This feature allows requests from trusted internal networks (e.g., from `leo_indexer`) to access S3 API without AWS Signature authentication.

## Background

### Problem Statement

- `leo_indexer` needs to access LeoFS Gateway via S3 API
- Managing `access_key` and `secret_access_key` for internal services adds complexity
- Both `leo_indexer` and `leo_gateway` operate within the same internal virtual network
- A simpler authentication bypass for internal communication is desired

### Requirements

1. **Bypass Scope**: All operations (GET/PUT/DELETE/HEAD)
2. **Bucket Scope**: All buckets accessible for internal requests
3. **Logging**: Internal access must be logged with identifiable markers

## Architecture

```
                    External Network
                          │
                          ▼
              ┌───────────────────────┐
              │    Load Balancer      │
              └───────────────────────┘
                          │
        ┌─────────────────┴─────────────────┐
        │                                   │
        ▼                                   ▼
┌───────────────┐                   ┌───────────────┐
│  External     │                   │  Internal     │
│  Client       │                   │  Network      │
│  (Internet)   │                   │  (10.0.0.0/8) │
└───────────────┘                   └───────────────┘
        │                                   │
        │ AWS Signature Required            │ No Auth Required
        │                                   │
        ▼                                   ▼
┌─────────────────────────────────────────────────────┐
│                    leo_gateway                      │
│  ┌───────────────────────────────────────────────┐  │
│  │              leo_gateway_s3_api               │  │
│  │  ┌─────────────────────────────────────────┐  │  │
│  │  │              auth/7                     │  │  │
│  │  │  1. Check: is_internal_network(IP)?     │  │  │
│  │  │     ├─ YES → Bypass auth                │  │  │
│  │  │     └─ NO  → Normal auth flow           │  │  │
│  │  └─────────────────────────────────────────┘  │  │
│  └───────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────┘
        │
        ▼
┌───────────────┐
│  leo_storage  │
└───────────────┘
```

## Detailed Design

### 1. Configuration

#### File: `priv/leo_gateway.conf`

```erlang
## --------------------------------------------------------------------
## GATEWAY - Internal Network (Authentication Bypass)
## --------------------------------------------------------------------
## Enable internal network authentication bypass
## When enabled, requests from specified CIDR ranges skip S3 authentication
## Default: false
internal_network.enabled = false

## CIDR ranges for internal network (comma-separated)
## Requests from these IP ranges will bypass authentication
## Common private network ranges:
##   - 10.0.0.0/8     : Class A private network
##   - 172.16.0.0/12  : Class B private network
##   - 192.168.0.0/16 : Class C private network
##   - 127.0.0.0/8    : Loopback
## Default: (empty - no bypass)
## internal_network.cidrs = 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16
```

#### Configuration Schema (`priv/leo_gateway.schema`)

```erlang
%% Internal Network Settings
{mapping, "internal_network.enabled", "leo_gateway.internal_network_enabled", [
    {datatype, {enum, [true, false]}},
    {default, false}
]}.

{mapping, "internal_network.cidrs", "leo_gateway.internal_network_cidrs", [
    {datatype, string},
    {default, ""}
]}.
```

### 2. Header File Changes

#### File: `include/leo_gateway.hrl`

```erlang
%%----------------------------------------------------------------------
%% INTERNAL NETWORK AUTHENTICATION BYPASS
%%----------------------------------------------------------------------
%% Special access_key_id for internal network requests
-define(INTERNAL_ACCESS_KEY_ID, <<"_internal_">>).

%% Environment macros for internal network settings
-define(env_internal_network_enabled(),
        case application:get_env(leo_gateway, internal_network_enabled) of
            {ok, true} -> true;
            _ -> false
        end).

-define(env_internal_network_cidrs(),
        case application:get_env(leo_gateway, internal_network_cidrs) of
            {ok, CIDRs} when is_list(CIDRs) -> CIDRs;
            _ -> []
        end).
```

### 3. CIDR Utility Module

#### File: `src/leo_gateway_cidr.erl` (New Module)

```erlang
-module(leo_gateway_cidr).

-export([parse_cidrs/1,
         is_in_cidrs/2]).

-type cidr() :: {inet:ip4_address(), non_neg_integer()}.

%% @doc Parse CIDR string list to internal format
%% Input: "10.0.0.0/8, 172.16.0.0/12"
%% Output: [{{10,0,0,0}, 8}, {{172,16,0,0}, 12}]
-spec parse_cidrs(string()) -> [cidr()].
parse_cidrs(CIDRStr) ->
    Tokens = string:tokens(CIDRStr, ", "),
    lists:filtermap(fun parse_cidr/1, Tokens).

%% @doc Parse single CIDR notation
-spec parse_cidr(string()) -> {true, cidr()} | false.
parse_cidr(CIDR) ->
    case string:tokens(string:trim(CIDR), "/") of
        [IPStr, MaskStr] ->
            case {inet:parse_address(IPStr),
                  catch list_to_integer(MaskStr)} of
                {{ok, IP}, Mask} when is_integer(Mask),
                                      Mask >= 0, Mask =< 32 ->
                    {true, {IP, Mask}};
                _ ->
                    false
            end;
        _ ->
            false
    end.

%% @doc Check if IP address is in any of the CIDR ranges
-spec is_in_cidrs(inet:ip4_address(), [cidr()]) -> boolean().
is_in_cidrs(IP, CIDRs) ->
    lists:any(fun(CIDR) -> is_in_cidr(IP, CIDR) end, CIDRs).

%% @doc Check if IP address is in CIDR range
-spec is_in_cidr(inet:ip4_address(), cidr()) -> boolean().
is_in_cidr({A, B, C, D}, {{NA, NB, NC, ND}, Mask}) ->
    IPInt = (A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D,
    NetInt = (NA bsl 24) bor (NB bsl 16) bor (NC bsl 8) bor ND,
    ShiftBits = 32 - Mask,
    (IPInt bsr ShiftBits) =:= (NetInt bsr ShiftBits);
is_in_cidr(_, _) ->
    %% IPv6 not supported yet
    false.
```

### 4. Authentication Flow Changes

#### File: `src/leo_gateway_s3_api.erl`

##### 4.1 Add Internal Network Check Function

```erlang
%%----------------------------------------------------------------------
%% Internal Network Authentication Bypass
%%----------------------------------------------------------------------

%% @doc Check if request is from internal network
-spec is_internal_network_request(cowboy_req:req()) -> boolean().
is_internal_network_request(Req) ->
    case ?env_internal_network_enabled() of
        false ->
            false;
        true ->
            case cowboy_req:peer(Req) of
                {IP, _Port} ->
                    CIDRs = ?env_internal_network_cidrs(),
                    leo_gateway_cidr:is_in_cidrs(IP, CIDRs);
                _ ->
                    false
            end
    end.
```

##### 4.2 Modify auth/7 Function

**Current Flow:**
```
auth/7 → ACL check → auth_1 (signature verification)
```

**New Flow:**
```
auth/7 → Internal network check → (bypass) OR → ACL check → auth_1
```

**Modified auth/7:**

```erlang
%% @doc Authentication with internal network bypass
auth(Req, HTTPMethod, Path, TokenLen, BucketName, ACLs, ReqParams) ->
    %% First: Check internal network bypass
    case is_internal_network_request(Req) of
        true ->
            %% Internal network: bypass authentication
            {ok, ?INTERNAL_ACCESS_KEY_ID, undefined};
        false ->
            %% Normal authentication flow
            auth_normal(Req, HTTPMethod, Path, TokenLen, BucketName, ACLs, ReqParams)
    end.

%% @doc Normal authentication (existing logic)
%% Rename existing auth/7 clauses to auth_normal/7
auth_normal(Req, HTTPMethod, Path, TokenLen, BucketName, ACLs,
            #req_params{is_multi_delete = true} = ReqParams) when TokenLen =< 1 ->
    %% ... existing code ...
```

### 5. Access Log Enhancement

#### File: `include/leo_gateway.hrl`

Add internal access log macros:

```erlang
%% Access log with internal network identifier
-define(access_log_get_internal(_Bucket, _Path, _Size, _Response, _Begin),
        begin
            {_OrgPath, _ChildNum} = ?get_child_num(binary_to_list(_Path)),
            _Clock = leo_date:clock(),
            _Latency = erlang:round((_Clock - _Begin) / 1000),
            logger:info("[GET][INTERNAL] ~s ~s ~w ~w ~s ~w ~w ~w",
                       [binary_to_list(_Bucket), _OrgPath, _ChildNum, _Size,
                        leo_date:date_format(), _Clock, _Response, _Latency],
                       #{domain => [leo_gateway, access_log]})
        end).

%% Similar macros for PUT, DELETE, HEAD...
```

#### Usage in Request Handlers

```erlang
%% In handle functions, check access_key_id to determine log format
case AccessKeyId of
    ?INTERNAL_ACCESS_KEY_ID ->
        ?access_log_get_internal(Bucket, Path, Size, Response, Begin);
    _ ->
        ?access_log_get(Bucket, Path, Size, Response, Begin)
end.
```

### 6. Application Startup

#### File: `src/leo_gateway_app.erl`

Initialize CIDR list at application startup:

```erlang
start(_Type, _Args) ->
    %% ... existing code ...

    %% Initialize internal network CIDR cache
    case ?env_internal_network_enabled() of
        true ->
            CIDRStr = application:get_env(leo_gateway, internal_network_cidrs_raw, ""),
            ParsedCIDRs = leo_gateway_cidr:parse_cidrs(CIDRStr),
            application:set_env(leo_gateway, internal_network_cidrs, ParsedCIDRs),
            ?info("Internal network bypass enabled",
                  [{cidrs, ParsedCIDRs}]);
        false ->
            ok
    end,

    %% ... rest of startup ...
```

## File Change Summary

| File | Change Type | Description |
|------|-------------|-------------|
| `priv/leo_gateway.conf` | Modify | Add `internal_network.*` settings |
| `priv/leo_gateway.schema` | Modify | Add schema mappings with CIDR parsing in translation |
| `include/leo_gateway.hrl` | Modify | Add macros, constants, and access log [INTERNAL] marker |
| `src/leo_gateway_cidr.erl` | **New** | CIDR matching utility with EUnit tests |
| `src/leo_gateway_s3_api.erl` | Modify | Add `is_internal_network_request/1` and modify `auth/5` |
| `src/leo_gateway_app.erl` | Modify | Add startup logging for internal network config |

## Security Considerations

### Risks

1. **Misconfiguration**: Accidentally allowing external IPs
2. **IP Spoofing**: In certain network configurations
3. **Proxy Headers**: X-Forwarded-For could be manipulated

### Mitigations

1. **Default Disabled**: Feature is disabled by default
2. **Private Networks Only**: Document that only RFC 1918 addresses should be used
3. **Direct Connection**: Use `cowboy_req:peer/1` which returns actual TCP peer, not proxy headers
4. **Logging**: All internal accesses are logged with `[INTERNAL]` marker for audit

### Recommended Configuration

```erlang
## Production: Only enable specific internal service networks
internal_network.enabled = true
internal_network.cidrs = 10.0.1.0/24

## NOT recommended: Too broad
## internal_network.cidrs = 10.0.0.0/8
```

## Testing Plan

### Unit Tests

1. `leo_gateway_cidr_tests.erl`
   - CIDR parsing (valid/invalid)
   - IP matching (in/out of range)
   - Edge cases (0.0.0.0/0, /32, etc.)

### Integration Tests

1. Internal network request → Auth bypassed
2. External network request → Normal auth required
3. Feature disabled → All requests require auth
4. Access logs contain `[INTERNAL]` marker

### Manual Testing

```bash
# From internal network (should succeed without credentials)
curl http://gateway:8080/bucket/object

# From external network (should fail without credentials)
curl http://gateway:8080/bucket/object
# Expected: 403 Forbidden
```

## Rollout Plan

1. **Phase 1**: Deploy with `internal_network.enabled = false`
2. **Phase 2**: Enable in staging with specific CIDR
3. **Phase 3**: Verify logs show `[INTERNAL]` markers
4. **Phase 4**: Enable in production

## Future Enhancements

1. **Per-bucket configuration**: Allow internal access only to specific buckets
2. **IPv6 support**: Extend CIDR matching to IPv6
3. **Rate limiting**: Apply different rate limits for internal vs external
4. **Metrics**: Separate metrics for internal/external requests

---

## Implementation Notes

### Critical Fix: `check_bad_date` Bypass (2025-01-20)

During implementation, a critical issue was discovered: requests were being rejected with 403 before the `auth` function was called.

**Root Cause**: The `check_bad_date` function in `leo_gateway_s3_api.erl` validates the `Date` header for requests with an `Authorization` header. When boto3 sends requests with `Authorization` but invalid/missing `Date` headers, the request was rejected before the internal network check in `auth/5` could execute.

**Request Flow**:

```
cowboy → handle/2 → check_request/1 → check_bad_date/1 → (REJECTED HERE)
                                                        ↓
                                              auth/5 never reached
```

**Fix**: Modified `check_bad_date/1` to skip date validation for internal network requests:

```erlang
check_bad_date(Req) ->
    case cowboy_req:header(?HTTP_HEAD_AUTHORIZATION, Req) of
        undefined ->
            ok;
        _ ->
            %% Skip date validation for internal network requests
            case is_internal_network_request(Req) of
                true ->
                    ok;
                false ->
                    check_bad_date_1(Req)
            end
    end.
```

### Docker Environment: Endpoint Registration Required

When using internal network bypass in Docker environments, the Gateway hostname must be registered as an endpoint. Otherwise, the hostname is incorrectly parsed as the bucket name.

**Example Issue**:

- Request: `GET http://leogateway:8080/bucket-1/object.txt`
- Without endpoint: Bucket = `leogateway`, Key = `leogateway/bucket-1/object.txt` ❌
- With endpoint: Bucket = `bucket-1`, Key = `object.txt` ✓

**Solution**:

```bash
# Register the Docker hostname as an endpoint
./leofs-adm add-endpoint leogateway

# Verify
./leofs-adm get-endpoints
```

### Files Modified

| File | Changes |
|------|---------|
| `src/leo_gateway_s3_api.erl` | Added `is_internal_network_request/1`, modified `check_bad_date/1` and `auth/5` |
| `src/leo_gateway_cidr.erl` | New module for CIDR matching utilities |
| `include/leo_gateway.hrl` | Added `?INTERNAL_ACCESS_KEY_ID` and env macros |
| `priv/leo_gateway.conf` | Added `internal_network.*` configuration |
| `priv/leo_gateway.schema` | Added schema mappings with CIDR parsing |
| `src/leo_gateway_app.erl` | Added CIDR parsing at startup |

### Verified Working Configuration

```erlang
## leo_gateway.conf
internal_network.enabled = true
internal_network.cidrs = 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16, 127.0.0.0/8
```

Docker Compose network (172.23.0.0/16) is within `172.16.0.0/12` range.
