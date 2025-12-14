# LeoFS Docker Compose

LeoFS cluster deployment using Docker Compose. Two configurations are available:

- **Single Storage**: 1 storage node (for development/testing)
- **Multiple Storage**: 4 storage nodes with replication (for production-like environments)

## Prerequisites

- Docker Engine 20.10+
- Docker Compose v2

## Quick Start

### Single Storage Configuration

```bash
# 1. Build Docker images (first time only)
cd docker
./build.sh

# 2. Start the cluster
docker compose up -d

# 3. Initialize the cluster
./init-cluster.sh
```

### Multiple Storage Configuration (4 nodes with replication)

```bash
# 1. Build Docker images (first time only)
cd docker
./build.sh

# 2. Start the cluster
docker compose -f docker-compose-multiple-storage.yml up -d

# 3. Initialize the cluster
./init-cluster-multi.sh
```

## Build Process

The `build.sh` script performs the following:

1. **Create leofs-builder image**: Builds Erlang/OTP 28 and LeoFS inside Docker
2. **Create component images**: Copies built packages to lightweight runtime images

The build works on macOS (including Apple Silicon) because it generates Linux binaries inside the Docker container.

## Cluster Configurations

### Single Storage (docker-compose.yml)

| Component | Replicas | Description |
|-----------|----------|-------------|
| leo_manager_0 | 1 | Primary manager |
| leo_manager_1 | 1 | Secondary manager |
| leo_storage | 1 | Storage node |
| leo_gateway | 1 | S3 API gateway |

**Consistency Level**: N=1, R=1, W=1, D=1

### Multiple Storage (docker-compose-multiple-storage.yml)

| Component | Replicas | Description |
|-----------|----------|-------------|
| leo_manager_0 | 1 | Primary manager |
| leo_manager_1 | 1 | Secondary manager |
| leo_storage_0-3 | 4 | Storage nodes |
| leo_gateway | 1 | S3 API gateway |

**Consistency Level**: N=3, R=1, W=2, D=2

## Usage

### Start Cluster

```bash
# Single storage
docker compose up -d

# Multiple storage
docker compose -f docker-compose-multiple-storage.yml up -d
```

### Stop Cluster

```bash
# Single storage
docker compose down

# Multiple storage
docker compose -f docker-compose-multiple-storage.yml down
```

### View Logs

```bash
# All services
docker compose logs -f

# Specific service
docker compose logs -f leo_gateway
```

### Check Status

```bash
../leofs-adm status
```

## Port Mapping

| Service | Host Port | Container Port | Purpose |
|---------|-----------|----------------|---------|
| leo_manager_0 | 10010 | 10010 | CUI Console |
| leo_manager_0 | 10020 | 10020 | JSON API |
| leo_manager_1 | 10012 | 10012 | CUI Console |
| leo_manager_1 | 10022 | 10022 | JSON API |
| leo_gateway | 8080 | 8080 | S3 API |

## S3 Client Connection

### AWS CLI

```bash
# Create user
../leofs-adm create-user testuser

# Configure AWS CLI with the output access_key_id and secret_access_key
aws configure --profile leofs
# AWS Access Key ID: <access_key_id>
# AWS Secret Access Key: <secret_access_key>
# Default region name: us-east-1
# Default output format: json

# Create bucket
../leofs-adm add-bucket test-bucket <access_key_id>

# Upload file
aws --profile leofs --endpoint-url http://localhost:8080 s3 cp test.txt s3://test-bucket/

# List files
aws --profile leofs --endpoint-url http://localhost:8080 s3 ls s3://test-bucket/
```

## Data Persistence

Data is stored in Docker volumes:

**Single Storage:**
- `manager0_work`, `manager1_work`: Mnesia data
- `storage_avs`: Object storage data
- `*_log`: Service logs

**Multiple Storage:**
- `manager0_work`, `manager1_work`: Mnesia data
- `storage0_avs` - `storage3_avs`: Object storage data (per node)
- `*_log`: Service logs

### Full Reset (Delete All Data)

```bash
# Single storage
docker compose down -v

# Multiple storage
docker compose -f docker-compose-multiple-storage.yml down -v
```

## File Structure

```
docker/
├── Dockerfile.builder                  # LeoFS build image
├── Dockerfile                          # Runtime image
├── docker-compose.yml                  # Single storage cluster
├── docker-compose-multiple-storage.yml # Multiple storage cluster
├── build.sh                            # Build script
├── entrypoint.sh                       # Container startup script
├── init-cluster.sh                     # Cluster initialization (single)
├── init-cluster-multi.sh               # Cluster initialization (multiple)
├── 99fixbadproxy                       # APT configuration fix
└── config/
    ├── leo_manager_0.conf              # Manager 0 config (single)
    ├── leo_manager_0_multi.conf        # Manager 0 config (multiple)
    ├── leo_manager_1.conf              # Manager 1 config
    ├── leo_storage.conf                # Storage config (single)
    ├── leo_storage_0.conf              # Storage 0 config (multiple)
    ├── leo_storage_1.conf              # Storage 1 config (multiple)
    ├── leo_storage_2.conf              # Storage 2 config (multiple)
    ├── leo_storage_3.conf              # Storage 3 config (multiple)
    └── leo_gateway.conf                # Gateway config
```

## Troubleshooting

### Node Fails to Start

```bash
# Check logs
docker compose logs leo_manager_0

# Debug inside container
docker compose exec leo_manager_0 /bin/bash
```

### Cluster Connection Error

Erlang nodes require matching `distributed_cookie` for inter-node communication.
Verify all configuration files have the same value (`401321b4`).

### Rebuild Images

```bash
# Rebuild from builder image
./build.sh

# Full rebuild without cache
docker build --no-cache -f Dockerfile.builder -t leofs-builder:latest ..
./build.sh
```

### Change Replication Settings

To modify consistency levels for multiple storage configuration, edit `config/leo_manager_0_multi.conf`:

```
consistency.num_of_replicas = 3
consistency.write = 2
consistency.read = 1
consistency.delete = 2
```

After changing, restart the cluster with volume deletion:

```bash
docker compose -f docker-compose-multiple-storage.yml down -v
docker compose -f docker-compose-multiple-storage.yml up -d
./init-cluster-multi.sh
```
