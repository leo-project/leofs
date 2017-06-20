# Log Management
## LeoGateway Access-log

### Overview

LeoGateway provides `access-log` output feature so that you can investigate requests from users.


### How To Output Access-log

Modify `access-log` configuration item which include `leo_gateway.conf`. After starting LeoGateway node(s), those nodes output `access-log` into the local disk, which is under each LeoGateway’s log directory. See more detail [For Administrators / Settings / LeoGateway Settings](/admin/settings/leo_gateway.md).

#### Access-log Configuration

```ini
## Is enable access-log [true, false]
log.is_enable_access_log = true
```

#### LeoGateway's Log Files

| Log File      | File Name   |
|--------------:|-------------|
| access-log    | /log/app/access.* |
| error-log     | /log/app/error.* |
| info-log      | /log/app/info.* |

```text
log/
├── [ 272 Apr 28 14:13]  app
│   ├── [  98 Apr 28 14:13]  access -> /leofs/package/leo_gateway/log/app/access.20170428.14.1
│   ├── [7.4K Apr 28 14:13]  access.20170428.14.1
│   ├── [  97 Apr 28 14:13]  error -> /leofs/package/leo_gateway/log/app/error.20170428.14.1
│   ├── [   0 Apr 28 14:13]  error.20170428.14.1
│   ├── [  96 Apr 28 14:13]  info -> /leofs/package/leo_gateway/log/app/info.20170428.14.1
│   └── [5.4K Apr 28 14:13]  info.20170428.14.1

```


### Example

```text
--------+-------+--------------------+----------+-------+---------------------------------------+-----------------------+----------
Method  | Bucket| Path               |Child Num |  Size | Timestamp                             | Unixtime              | Response
--------+-------+--------------------+----------|-------+---------------------------------------+-----------------------+----------
[HEAD]   photo   photo/1              0          0       2013-10-18 13:28:56.148269 +0900        1381206536148320        500
[HEAD]   photo   photo/1              0          0       2013-10-18 13:28:56.465670 +0900        1381206536465735        404
[HEAD]   photo   photo/city/tokyo.png 0          0       2013-10-18 13:28:56.489234 +0900        1381206536489289        200
[GET]    photo   photo/1              0          1024    2013-10-18 13:28:56.518631 +0900        1381206536518693        500
[GET]    photo   photo/city/paris.png 0          2048    2013-10-18 13:28:56.550376 +0900        1381206536550444        404
[PUT]    logs    logs/leofs           1          5242880 2013-10-18 13:28:56.518631 +0900        1381206536518693        500
[PUT]    logs    logs/leofs           2          5242880 2013-10-18 13:28:56.518631 +0900        1381206536518693        500
[PUT]    logs    logs/leofs           3          5120    2013-10-18 13:28:56.518631 +0900        1381206536518693        500
```

### Format

An access-log's format is TSV, *Tab Separated Values*.

| Column Number | Description |
|--------------:|-------------|
| 1             | Method: [HEAD|PUT|GET|DELETE] |
| 2             | Bucket |
| 3             | Filename (including path) |
| 4             | Child number of a file |
| 5             | File Size (byte) |
| 6             | Timestamp with timezone |
| 7             | Unixtime (including micro-second) |
| 8             | Response (HTTP Status Code) |


## LeoStorage Data Diagnosis-log

### Overview

LeoStorage provides `diagnosis-log` output feature so that you can investigate a LeoStorage's data. If you would like to turn on its feature, you need to modify `leo_storage.conf` which contain LeoStorage's package.

### How To Output Diagnosis-log

Modify `diagnosis-log` configuration item which include `leo_storage.conf`. After executing the `diagnose-start` command, a specified node outputs `diagnosis-log` into the local disk, which is under each LeoStorage’s AVS directory. See more detail [For Administrators / Settings / LeoStorage Settings](/admin/settings/leo_storage.md).

Execute the `diagnose-start` command with a specified LeoStorage node, then its LeoStorage output diagnosis-log(s) into the local disk, which is under its LeoStorage’s log directory. You can configure its log directory. See more detail [For Administrators / System Operations / Data Operations - Diagnosis](/admin/system_operations/data/#diagnosis) and [For Administrators / Settings / LeoStorage Settings](/admin/settings/leo_storage.md).

```bash
$ leofs-adm diagnose-start <storage-node>
```

#### Diagnosis-log Configuration

```ini
## Output data-diagnosis log
log.is_enable_diagnosis_log = true
```

#### LeoStorage's Log Files

| Log File      | File Name   |
|--------------:|-------------|
| access-log    | /log/app/access.* |
| diagnosis-log | /avs/log/leo_object_storage_*.* |
| error-log     | /log/app/error.* |
| info-log      | /log/app/info.* |

```text
## diagnosis-log files
/leofs/package/leo_storage/avs/log/
├── [ 106 Apr 28 14:40]  leo_object_storage_0 -> /leofs/package/leo_storage/avs/log/leo_object_storage_0.20170428.14.2
├── [ 683 Apr 28 14:40]  leo_object_storage_0.20170428.14.1
├── [   0 Apr 28 14:40]  leo_object_storage_0.20170428.14.2
├── [ 366 Apr 28 14:40]  leo_object_storage_0.report.63660577222
├── [ 106 Apr 28 14:40]  leo_object_storage_1 -> /leofs/package/leo_storage/avs/log/leo_object_storage_1.20170428.14.2
├── [ 779 Apr 28 14:40]  leo_object_storage_1.20170428.14.1
├── [   0 Apr 28 14:40]  leo_object_storage_1.20170428.14.2
├── [ 366 Apr 28 14:40]  leo_object_storage_1.report.63660577222
├── [ 106 Apr 28 14:40]  leo_object_storage_2 -> /leofs/package/leo_storage/avs/log/leo_object_storage_2.20170428.14.2
├── [ 786 Apr 28 14:40]  leo_object_storage_2.20170428.14.1
├── [   0 Apr 28 14:40]  leo_object_storage_2.20170428.14.2
├── [ 368 Apr 28 14:40]  leo_object_storage_2.report.63660577223
├── [ 106 Apr 28 14:40]  leo_object_storage_3 -> /leofs/package/leo_storage/avs/log/leo_object_storage_3.20170428.14.2
├── [1.3K Apr 28 14:40]  leo_object_storage_3.20170428.14.1
├── [   0 Apr 28 14:40]  leo_object_storage_3.20170428.14.2
├── [ 366 Apr 28 14:40]  leo_object_storage_3.report.63660577223
├── [ 106 Apr 28 14:40]  leo_object_storage_4 -> /leofs/package/leo_storage/avs/log/leo_object_storage_4.20170428.14.2
├── [1.3K Apr 28 14:40]  leo_object_storage_4.20170428.14.1
├── [   0 Apr 28 14:40]  leo_object_storage_4.20170428.14.2
├── [ 368 Apr 28 14:40]  leo_object_storage_4.report.63660577224
├── [ 106 Apr 28 14:40]  leo_object_storage_5 -> /leofs/package/leo_storage/avs/log/leo_object_storage_5.20170428.14.2
├── [ 943 Apr 28 14:40]  leo_object_storage_5.20170428.14.1
├── [   0 Apr 28 14:40]  leo_object_storage_5.20170428.14.2
├── [ 366 Apr 28 14:40]  leo_object_storage_5.report.63660577225
├── [ 106 Apr 28 14:40]  leo_object_storage_6 -> /leofs/package/leo_storage/avs/log/leo_object_storage_6.20170428.14.2
├── [1.5K Apr 28 14:40]  leo_object_storage_6.20170428.14.1
├── [   0 Apr 28 14:40]  leo_object_storage_6.20170428.14.2
├── [ 368 Apr 28 14:40]  leo_object_storage_6.report.63660577225
├── [ 106 Apr 28 14:40]  leo_object_storage_7 -> /leofs/package/leo_storage/avs/log/leo_object_storage_7.20170428.14.2
├── [1.1K Apr 28 14:40]  leo_object_storage_7.20170428.14.1
├── [   0 Apr 28 14:40]  leo_object_storage_7.20170428.14.2
└── [ 368 Apr 28 14:40]  leo_object_storage_7.report.63660577226

0 directories, 32 files
```


### Example

```text
## Example:
------+------------------------------------------+------------------------------------------------------------+-----------+------------+------------------+--------------------------+----
Offset| RING's address-id                        | Filename                                                   | Child num | File Size  | Unixtime         | Localtime                |del?
------+------------------------------------------+------------------------------------------------------------+-----------+------------+------------------+--------------------------+----
194     296754181484029444656944009564610621293   photo/leo_redundant_manager/Makefile                             0       2034        1413348050768344   2014-10-15 13:40:50 +0900   0
2400    185993533055981727582172380494809056426   photo/leo_redundant_manager/ebin/leo_redundant_manager.beam      0       24396       1413348050869454   2014-10-15 13:40:50 +0900   0
38446   53208912738248114804281793572563205919    photo/leo_rpc/.git/refs/remotes/origin/HEAD                      0       33          1413348057441546   2014-10-15 13:40:57 +0900   0
38658   57520977797167422772945547576980778561    photo/leo_rpc/ebin/leo_rpc_client_utils.beam                     0       2576        1413348057512261   2014-10-15 13:40:57 +0900   0
69506   187294034498591995039607573685274229706   photo/leo_backend_db/src/leo_backend_db_server.erl               0       13911       1413348068031188   2014-10-15 13:41:08 +0900   0
83603   316467020376888598364250682951088839795   photo/leo_backend_db/test/leo_backend_db_api_prop.erl            0       3507        1413348068052219   2014-10-15 13:41:08 +0900   1
```

### Format

A diagnose-log's format is TSV, *Tab Separated Values*.

| Column Number | Description |
|--------------:|-------------|
| 1             | Offset of the AVS-file |
| 2             | RING’s address id (routing-table) |
| 3             | Filename |
| 4             | Child number of a file |
| 5             | File Size (byte) |
| 6             | Timestamp - unixtime |
| 7             | Timestamp - localtime |
| 8             | Removed file? |


## Related Links

- [For Administrators / Settings / LeoGateway Settings](/admin/settings/leo_gateway.md)
- [For Administrators / Settings / LeoStorage Settings](/admin/settings/leo_storage.md)
- [For Administrators / System Operations / Data Operations - Diagnosis](/admin/system_operations/data/#diagnosis)
