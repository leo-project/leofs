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

## Project Structure

```
src/
├── __init__.py
├── config.py           # Configuration management
├── exceptions.py       # Custom exceptions
├── models.py           # Pydantic models
├── main.py             # Entry point
├── consumer.py         # NATS JetStream Consumer
├── s3_client.py        # LeoFS S3 API Client
├── lancedb_handler.py  # LanceDB operations
├── task_index.py       # Pipeline facade
│
├── text/               # Text processing modules
│   ├── language.py     # Language detection (fasttext)
│   ├── preprocessor.py # Text normalization
│   ├── chunking.py     # Document chunking (LlamaIndex)
│   └── embedding.py    # Embedding generation
│
└── pipelines/          # Pipeline implementations
    ├── base.py         # Base pipeline class
    ├── simple.py       # Phase 1: Text only
    └── full.py         # Phase 2: With embeddings
```

## Components

- **consumer.py** - NATS JetStream Pull Consumer
- **s3_client.py** - LeoFS S3 API Client (boto3)
- **lancedb_handler.py** - LanceDB table creation
- **task_index.py** - Pipeline facade
- **main.py** - Entry point
- **text/** - Text processing (language detection, preprocessing, chunking, embeddings)
- **pipelines/** - Pipeline implementations (simple, full)

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
python -m src.main
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

## Phase 2: Advanced Pipeline (Language Detection, Chunking, Embeddings)

### Pipeline Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Pipeline Flow                            │
└─────────────────────────────────────────────────────────────────┘

  Upload Event (NATS)
         │
         ▼
  ┌──────────────────┐
  │  TaskIndex       │
  │  (Orchestrator)  │
  └────────┬─────────┘
           │
           ▼
  ┌──────────────────┐     ┌─────────────────────┐
  │ LanguageRouter   │────▶│ lang: ja / en       │
  │ (fasttext)       │     │ unsupported → skip  │
  └────────┬─────────┘     └─────────────────────┘
           │
           ▼
  ┌──────────────────┐
  │ TextPreprocessor │  ← Language-specific normalization
  │ (ja/en)          │
  └────────┬─────────┘
           │
           ▼
  ┌──────────────────┐
  │ ChunkingService  │  ← LlamaIndex SentenceSplitter
  │ (LlamaIndex)     │
  └────────┬─────────┘
           │
           ▼
  ┌──────────────────┐
  │ EmbeddingService │  ← multilingual-e5-base (768 dims)
  │ (sentence-trans) │
  └────────┬─────────┘
           │
           ▼
  ┌──────────────────┐
  │ LanceDBHandler   │  ← Extended schema with vectors
  │ (vector storage) │
  └──────────────────┘
```

### Text Processing Modules (`src/text/`)

| File | Description |
|------|-------------|
| `language.py` | Language detection using fasttext (lid.176.ftz) |
| `preprocessor.py` | Text preprocessing (Unicode normalization, cleanup) |
| `chunking.py` | Document chunking using LlamaIndex |
| `embedding.py` | Embedding generation using sentence-transformers |

### Pipeline Implementations (`src/pipelines/`)

| File | Description |
|------|-------------|
| `base.py` | Base pipeline class with shared utilities |
| `simple.py` | Phase 1 pipeline (text storage only) |
| `full.py` | Phase 2 pipeline (with language detection & embeddings) |

### Phase 2 Configuration

| Variable | Default | Description |
|----------|---------|-------------|
| `FASTTEXT_MODEL_PATH` | `models/lid.176.ftz` | Path to fasttext language detection model |
| `LANGUAGE_DETECTION_CHARS` | `1000` | Number of characters to use for language detection |
| `LANGUAGE_CONFIDENCE_THRESHOLD` | `0.5` | Minimum confidence for language detection |
| `SUPPORTED_LANGUAGES` | `ja,en` | Comma-separated list of supported languages |
| `CHUNK_SIZE` | `512` | Target chunk size in tokens |
| `CHUNK_OVERLAP` | `50` | Overlap between chunks in tokens |
| `EMBEDDING_MODEL` | `intfloat/multilingual-e5-base` | Default embedding model (fallback) |
| `EMBEDDING_MODEL_JA` | `cl-nagoya/ruri-v3-pt-30m` | Embedding model for Japanese text |
| `EMBEDDING_MODEL_EN` | `intfloat/multilingual-e5-base` | Embedding model for English text |
| `EMBEDDING_BATCH_SIZE` | `32` | Batch size for embedding generation |
| `EMBEDDING_DIMENSION` | `768` | Default embedding dimension |
| `EMBEDDING_DIMENSION_JA` | `256` | Embedding dimension for Japanese model |
| `EMBEDDING_DIMENSION_EN` | `768` | Embedding dimension for English model |
| `PIPELINE_MODE` | `full` | Pipeline mode: 'simple' or 'full' |

### Embedding Models

Language-specific embedding models are used for optimal performance:

| Language | Model | Dimension | Description |
|----------|-------|-----------|-------------|
| Japanese | [cl-nagoya/ruri-v3-pt-30m](https://huggingface.co/cl-nagoya/ruri-v3-pt-30m) | 256 | Lightweight Japanese embedding model |
| English | [intfloat/multilingual-e5-base](https://huggingface.co/intfloat/multilingual-e5-base) | 768 | Multilingual E5 model |

Models are loaded lazily on first use for each language.

### Extended LanceDB Schema

| Field | Type | Description |
|-------|------|-------------|
| id | string | `{etag}_{chunk_id}` (composite key) |
| bucket | string | Source bucket name |
| key | string | Source object key |
| etag | string | Original file ETag |
| chunk_id | int | Chunk index (0-based) |
| content | string | Chunk text content |
| vector | vector(varies) | Embedding vector (dimension varies by language) |
| language | string | Detected language code |
| created_at | int64 | Unix timestamp |

### fasttext Language Detection Model

Download the compressed model (recommended):

```bash
mkdir -p models
curl -L -o models/lid.176.ftz https://dl.fbaipublicfiles.com/fasttext/supervised-models/lid.176.ftz
```

| Model | Size | Description |
|-------|------|-------------|
| `lid.176.bin` | ~126MB | Full model, higher accuracy |
| `lid.176.ftz` | ~917KB | Compressed, lightweight (recommended) |

### Dependencies (Phase 2)

```
# requirements.txt additions
fasttext>=0.9.2             # Language detection
llama-index-core>=0.10.0    # Chunking
sentence-transformers>=2.2.0 # Embeddings
torch>=2.0.0                # PyTorch backend
```
