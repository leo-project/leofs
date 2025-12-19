#!/bin/bash
#
# LeoFS S3 API Test Script
# Usage: ./run_s3_test.sh <username> <bucket-name>
#

set -e

# Default values
ENDPOINT="http://127.0.0.1:28080"
LEOFS_ADM="./leofs-adm"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

usage() {
    echo "Usage: $0 <username> <bucket-name>"
    echo ""
    echo "Example:"
    echo "  $0 test-001 test-bucket"
    exit 1
}

# Check arguments
if [ $# -lt 2 ]; then
    usage
fi

USERNAME="$1"
BUCKET_NAME="$2"

echo -e "${YELLOW}=== LeoFS S3 API Test Setup ===${NC}"
echo "Username:    $USERNAME"
echo "Bucket:      $BUCKET_NAME"
echo "Endpoint:    $ENDPOINT"
echo ""

# Step 1: Create user
echo -e "${YELLOW}[1/4] Creating user: $USERNAME${NC}"
USER_OUTPUT=$($LEOFS_ADM create-user "$USERNAME" 2>&1)

# Parse access-key-id and secret-access-key from output
# Use tr -d '\r' to remove carriage return characters
ACCESS_KEY=$(echo "$USER_OUTPUT" | grep "access-key-id:" | awk '{print $2}' | tr -d '\r')
SECRET_KEY=$(echo "$USER_OUTPUT" | grep "secret-access-key:" | awk '{print $2}' | tr -d '\r')

if [ -z "$ACCESS_KEY" ] || [ -z "$SECRET_KEY" ]; then
    echo -e "${RED}Failed to create user or parse credentials${NC}"
    echo "Output: $USER_OUTPUT"
    exit 1
fi

echo -e "${GREEN}  access-key-id:     $ACCESS_KEY${NC}"
echo -e "${GREEN}  secret-access-key: $SECRET_KEY${NC}"

# Step 2: Add bucket
echo -e "${YELLOW}[2/4] Creating bucket: $BUCKET_NAME${NC}"
$LEOFS_ADM add-bucket "$BUCKET_NAME" "$ACCESS_KEY"

# Step 3: Add endpoint (ignore if already exists)
echo -e "${YELLOW}[3/4] Adding endpoint: 127.0.0.1${NC}"
$LEOFS_ADM add-endpoint 127.0.0.1 2>/dev/null || echo "  (endpoint may already exist)"

# Step 4: Run S3 test
echo -e "${YELLOW}[4/4] Running S3 boto3 test${NC}"
echo ""

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
python3 "$SCRIPT_DIR/test_s3_boto3_debug.py" \
    --endpoint "$ENDPOINT" \
    --bucket "$BUCKET_NAME" \
    --access-key "$ACCESS_KEY" \
    --secret-key "$SECRET_KEY" \
    --no-expect \
    --no-debug

echo ""
echo -e "${GREEN}=== Test completed ===${NC}"
