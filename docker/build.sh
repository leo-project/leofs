#!/bin/bash
#======================================================================
# LeoFS Docker Build Script
#======================================================================
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
LEOFS_DIR="$(dirname "$SCRIPT_DIR")"

echo "=========================================="
echo " LeoFS Docker Build"
echo "=========================================="

cd "$LEOFS_DIR"

# Step 1: Build the builder image (compiles LeoFS)
echo ""
echo "[1/2] Building leofs-builder image (this may take 10-20 minutes)..."
docker build -f docker/Dockerfile.builder -t leofs-builder:latest .

# Step 2: Build runtime images for each component
echo ""
echo "[2/2] Building runtime images..."

for COMPONENT in leo_manager_0 leo_manager_1 leo_storage leo_gateway; do
    echo "  Building ${COMPONENT}..."
    docker build -f docker/Dockerfile \
        --build-arg COMPONENT=${COMPONENT} \
        -t leofs-${COMPONENT}:latest .
done

echo ""
echo "=========================================="
echo " Build Complete!"
echo "=========================================="
echo ""
echo "Images created:"
docker images | grep leofs
echo ""
echo "Next steps:"
echo "  cd docker"
echo "  docker compose up -d"
echo "  ./init-cluster.sh"
