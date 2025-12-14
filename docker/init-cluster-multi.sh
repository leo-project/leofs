#!/bin/bash
#======================================================================
# LeoFS Docker Cluster Initialization Script (Multiple Storage)
#======================================================================
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
LEOFS_ADM="${SCRIPT_DIR}/../leofs-adm"

echo "=========================================="
echo " LeoFS Docker Cluster Initialization"
echo " (Multiple Storage Configuration)"
echo "=========================================="

# Check if leofs-adm exists
if [ ! -f "$LEOFS_ADM" ]; then
    echo "Error: leofs-adm not found at $LEOFS_ADM"
    exit 1
fi

# Wait for all services to be healthy
echo ""
echo "[1/4] Waiting for services to be ready..."
sleep 5

# Check manager status
echo ""
echo "[2/4] Checking cluster status..."
$LEOFS_ADM -p 10010 status || {
    echo "Warning: Could not connect to manager. Retrying in 5 seconds..."
    sleep 5
    $LEOFS_ADM -p 10010 status
}

# Start the cluster
echo ""
echo "[3/4] Starting the cluster..."
$LEOFS_ADM -p 10010 start

# Wait and verify
echo ""
echo "[4/4] Verifying cluster status..."
sleep 3
$LEOFS_ADM -p 10010 status

echo ""
echo "=========================================="
echo " LeoFS Cluster is Ready!"
echo "=========================================="
echo ""
echo "S3 API Endpoint: http://localhost:8080"
echo "Manager CUI:     localhost:10010"
echo ""
echo "To create a user:"
echo "  $LEOFS_ADM -p 10010 create-user <user-id>"
echo ""
echo "To add a bucket:"
echo "  $LEOFS_ADM -p 10010 add-bucket <bucket-name> <access-key-id>"
echo ""
