# leo_indexer

LeoFS Indexing Worker - Processes upload events from NATS JetStream and creates LanceDB indexes.

## Architecture

```
NATS JetStream  -->  leo_indexer  -->  LanceDB (.lance)  -->  LeoFS (S3 API)
     |                    |                                        |
     |                    |                                        |
leofs.events.upload      task.index                    {bucket}/.vectors/{etag}.lance.tar.gz
```

**Storage Format**: LanceDB indexes are stored in the same bucket under `.vectors/` directory (managed by the system).

## Components

- **consumer.py** - NATS JetStream Pull Consumer
- **s3_client.py** - LeoFS S3 API Client (boto3)
- **lancedb_handler.py** - LanceDB table creation
- **task_index.py** - Core indexing logic
- **main.py** - Entry point

## Authentication

leo_indexer supports two authentication methods:

### Option 1: Internal Network Bypass (Recommended for Docker)

When leo_gateway and leo_indexer are in the same Docker network, internal network authentication bypass can be used. This is the recommended approach for containerized deployments.

**Requirements:**
1. `internal_network.enabled = true` in leo_gateway.conf
2. Docker network CIDR must be included in `internal_network.cidrs`
3. The leo_gateway hostname must be registered as an endpoint

```bash
# Register the Docker hostname as an endpoint
./leofs-adm add-endpoint leogateway

# Verify
./leofs-adm get-endpoints
```

With this configuration, leo_indexer can access LeoFS without explicit credentials.

### Option 2: S3 Credentials

For external access or when internal network bypass is not available:

```bash
# Connect to LeoFS Manager
telnet localhost 10010

# Create indexer user
create-user indexer

# Note the access_key and secret_key from the output
```

Set credentials via environment variables:

```bash
export LEOFS_ACCESS_KEY=ABCDEFGHIJ1234567890
export LEOFS_SECRET_KEY=abcdefghij1234567890abcdefghij1234567890
```

## Configuration

Environment variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `NATS_URL` | `nats://localhost:4222` | NATS server URL |
| `NATS_STREAM` | `LEOFS_EVENTS` | JetStream stream name |
| `NATS_SUBJECT` | `leofs.events.upload` | Subject to subscribe |
| `LEOFS_ENDPOINT` | `http://localhost:8080` | LeoFS S3 API endpoint |
| `LEOFS_ACCESS_KEY` | - | LeoFS access key (required) |
| `LEOFS_SECRET_KEY` | - | LeoFS secret key (required) |
| `LOG_LEVEL` | `INFO` | Log level |

## Running with Docker Compose

```bash
# Set credentials (create user via LeoFS Manager first)
export LEOFS_ACCESS_KEY=your-access-key
export LEOFS_SECRET_KEY=your-secret-key

# Start with NATS profile (includes nats, leo_nats_bridge, leo_indexer)
cd /path/to/leofs/docker
docker compose --profile nats up -d

# View logs
docker logs -f leo_indexer
```

Credentials are passed to the container via environment variables defined in `docker-compose.yml`.

## Local Development

```bash
# Install dependencies
pip install -r requirements.txt

# Set environment variables
export NATS_URL=nats://localhost:4222
export LEOFS_ENDPOINT=http://localhost:8080
export LEOFS_ACCESS_KEY=your-key
export LEOFS_SECRET_KEY=your-secret

# Run
python -m src.pipeline.main
```

## Testing

```bash
# Prerequisites: Create a bucket using the indexer's credentials
aws --endpoint-url=http://localhost:28080 s3 mb s3://test-bucket

# Upload a test file
echo "Hello World" | aws --endpoint-url=http://localhost:28080 s3 cp - s3://test-bucket/hello.txt

# Wait for indexing (check logs: docker logs -f leo_indexer)

# Check .vectors/ directory in the same bucket
aws --endpoint-url=http://localhost:28080 s3 ls s3://test-bucket/.vectors/
```

Note: The indexer needs read/write access to both the source object and the `.vectors/` directory in the same bucket.

## LanceDB Schema

| Field | Type | Description |
|-------|------|-------------|
| id | string | ETag (primary key for idempotency) |
| bucket | string | Source bucket name |
| key | string | Source object key |
| content | string | Text content |
| created_at | int64 | Unix timestamp |

## Error Handling

- **Retryable errors**: Network issues, service unavailable (retry with backoff)
- **Permanent errors**: Invalid data, object not found (send to DLQ)
- **DLQ Subject**: `leofs.events.dlq`


---
## MEMO

認証設定の調査完了
LeoGateway の認証コード (leo_s3_auth.erl, leo_gateway_s3_api.erl) を調査した結果: 現状の認証フロー:
auth/5 → auth/7 → auth_1 → leo_s3_auth:authenticate
ACL (public-read, public-read-write) チェック後に S3 署名認証を実行
IP ベースの信頼設定は未実装
Phase 1 での決定: LeoGateway のコア変更は侵襲的なため、既存の S3 認証メカニズムを使用します。 README.md に追加した内容:
Authentication セクション: LeoFS Manager での専用ユーザー作成手順
Docker Compose: 環境変数での認証情報設定
Testing: バケット作成と認証に関する注記
セットアップ手順:

# 1. LeoFS Manager でユーザー作成
telnet localhost 10010
create-user indexer

# 2. 環境変数設定
export LEOFS_ACCESS_KEY=<access_key>
export LEOFS_SECRET_KEY=<secret_key>

# 3. Docker Compose 起動
docker compose --profile nats up -d
今後の拡張案 (Phase 2 以降):
LeoGateway に trusted_networks 設定を追加
Docker 内部ネットワークからのアクセスは認証スキップ