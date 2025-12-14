# LeoFS Docker Compose

A 4-node LeoFS cluster (leo_manager_0, leo_manager_1, leo_gateway, leo_storage) using Docker Compose.

## Prerequisites

- Docker Engine 20.10+
- Docker Compose v2

## Quick Start

```bash
# 1. Build Docker images (first time only, takes 10-20 minutes)
cd docker
./build.sh

# 2. Start the cluster
docker compose up -d

# 3. Initialize the cluster
./init-cluster.sh
```

## Build Process

The `build.sh` script performs the following:

1. **Create leofs-builder image**: Builds Erlang/OTP 28 and LeoFS inside Docker
2. **Create component images**: Copies built packages to lightweight runtime images

The build works on macOS (including Apple Silicon) because it generates Linux binaries inside the Docker container.

## Usage

### Start Cluster

```bash
cd docker
docker compose up -d
```

### Stop Cluster

```bash
docker compose down
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
../leofs-adm -p 20010 status
```

## Port Mapping

| Service | Host Port | Container Port | Purpose |
|---------|-----------|----------------|---------|
| leo_manager_0 | 20010 | 10010 | CUI Console |
| leo_manager_0 | 20020 | 10020 | JSON API |
| leo_manager_1 | 20012 | 10012 | CUI Console |
| leo_manager_1 | 20022 | 10022 | JSON API |
| leo_gateway | 28080 | 8080 | S3 API |

Note: Host ports are offset to avoid conflicts with native LeoFS installations.

## S3 Client Connection

### AWS CLI

```bash
# Create user
../leofs-adm -p 20010 create-user testuser

# Configure AWS CLI with the output access_key_id and secret_access_key
aws configure --profile leofs
# AWS Access Key ID: <access_key_id>
# AWS Secret Access Key: <secret_access_key>
# Default region name: us-east-1
# Default output format: json

# Create bucket
../leofs-adm -p 20010 add-bucket test-bucket <access_key_id>

# Upload file
aws --profile leofs --endpoint-url http://localhost:28080 s3 cp test.txt s3://test-bucket/

# List files
aws --profile leofs --endpoint-url http://localhost:28080 s3 ls s3://test-bucket/
```

## Data Persistence

Data is stored in the following Docker volumes:

- `manager0_work`, `manager1_work`: Mnesia data
- `storage_avs`: Object storage data
- `*_log`: Service logs

### Full Reset (Delete All Data)

```bash
docker compose down -v
```

## File Structure

```
docker/
├── Dockerfile.builder   # LeoFS build image
├── Dockerfile           # Runtime image
├── docker-compose.yml   # Cluster definition
├── build.sh             # Build script
├── entrypoint.sh        # Container startup script
├── init-cluster.sh      # Cluster initialization script
├── 99fixbadproxy        # APT configuration fix
└── config/
    ├── leo_manager_0.conf
    ├── leo_manager_1.conf
    ├── leo_storage.conf
    └── leo_gateway.conf
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
