#!/bin/bash
#
# LeoFS Batch Upload Test Script
# Creates a user, bucket, uploads test files, and lists objects.
#
# Usage: ./test_batch_upload.sh <username> <bucket-name> [file-count=100]
#

set -e

# Default values
ENDPOINT="http://127.0.0.1:28080"
LEOFS_ADM="./leofs-adm"
DEFAULT_FILE_COUNT=100
DEFAULT_WAIT_SEC=10

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

usage() {
    echo "Usage: $0 <username> <bucket-name> [file-count] [--wait <seconds>] [--debug]"
    echo ""
    echo "Arguments:"
    echo "  username        User name to create"
    echo "  bucket-name     Bucket name to create"
    echo "  file-count      Number of files to upload (default: $DEFAULT_FILE_COUNT)"
    echo "  --wait <sec>    Wait time in seconds before listing objects (default: $DEFAULT_WAIT_SEC)"
    echo "  --debug         Enable debug output for list_objects"
    echo ""
    echo "Example:"
    echo "  $0 test-user test-bucket"
    echo "  $0 test-user test-bucket 50"
    echo "  $0 test-user test-bucket 50 --wait 20"
    echo "  $0 test-user test-bucket 50 --wait 20 --debug"
    exit 1
}

# Check arguments
if [ $# -lt 2 ]; then
    usage
fi

USERNAME="$1"
BUCKET_NAME="$2"
FILE_COUNT="${3:-$DEFAULT_FILE_COUNT}"
WAIT_SEC="$DEFAULT_WAIT_SEC"
DEBUG_FLAG=""

# If $3 is a flag, reset FILE_COUNT to default
if [[ "$3" == --* ]]; then
    FILE_COUNT="$DEFAULT_FILE_COUNT"
fi

# Parse optional flags
shift 2  # Skip username and bucket-name
while [ $# -gt 0 ]; do
    case "$1" in
        --wait)
            WAIT_SEC="$2"
            shift 2
            ;;
        --debug)
            DEBUG_FLAG="--debug"
            shift
            ;;
        *)
            # Positional argument (file-count)
            if [[ ! "$1" == --* ]]; then
                FILE_COUNT="$1"
            fi
            shift
            ;;
    esac
done

echo -e "${YELLOW}========================================${NC}"
echo -e "${YELLOW}  LeoFS Batch Upload Test${NC}"
echo -e "${YELLOW}========================================${NC}"
echo "  Username:    $USERNAME"
echo "  Bucket:      $BUCKET_NAME"
echo "  File count:  $FILE_COUNT"
echo "  Wait:        ${WAIT_SEC}s"
echo "  Endpoint:    $ENDPOINT"
echo "  Debug:       ${DEBUG_FLAG:-off}"
echo ""

# Step 1: Create user
echo -e "${YELLOW}[1/4] Creating user: $USERNAME${NC}"
USER_OUTPUT=$($LEOFS_ADM create-user "$USERNAME" 2>&1)

# Parse access-key-id and secret-access-key from output
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
echo -e "${GREEN}  Bucket created successfully${NC}"

# Step 3: Add endpoint (ignore if already exists)
echo -e "${YELLOW}[3/4] Adding endpoint: 127.0.0.1${NC}"
$LEOFS_ADM add-endpoint 127.0.0.1 2>/dev/null || echo "  (endpoint may already exist)"

# Step 4: Run batch upload
echo -e "${YELLOW}[4/4] Running batch upload test${NC}"
echo ""

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
python3 "$SCRIPT_DIR/batch_upload.py" \
    --endpoint "$ENDPOINT" \
    --bucket "$BUCKET_NAME" \
    --access-key "$ACCESS_KEY" \
    --secret-key "$SECRET_KEY" \
    --count "$FILE_COUNT" \
    --wait "$WAIT_SEC" \
    $DEBUG_FLAG

echo ""
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}  Test completed${NC}"
echo -e "${GREEN}========================================${NC}"
