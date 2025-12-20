#!/bin/bash
#======================================================================
# List objects in a LeoFS bucket via leo_indexer container
#
# Usage:
#   ./list-objects.sh <bucket-name> [prefix]
#
# Examples:
#   ./list-objects.sh bucket-6
#   ./list-objects.sh bucket-6 .vectors/
#======================================================================

set -e

BUCKET="${1:-}"
PREFIX="${2:-}"

if [ -z "$BUCKET" ]; then
    echo "Usage: $0 <bucket-name> [prefix]"
    echo ""
    echo "Examples:"
    echo "  $0 bucket-6"
    echo "  $0 bucket-6 .vectors/"
    exit 1
fi

# Check if leo_indexer container is running
if ! docker ps --format '{{.Names}}' | grep -q '^leo_indexer$'; then
    echo "Error: leo_indexer container is not running"
    echo "Start it with: docker compose --profile nats up -d"
    exit 1
fi

docker exec leo_indexer python3 -c "
import boto3
from botocore.config import Config
import os
import sys

s3 = boto3.client(
    's3',
    endpoint_url=os.environ.get('LEOFS_ENDPOINT', 'http://leogateway:8080'),
    aws_access_key_id='dummy',
    aws_secret_access_key='dummy',
    config=Config(signature_version='s3', s3={'addressing_style': 'path'})
)

bucket = '$BUCKET'
prefix = '$PREFIX'

try:
    all_objects = []

    if prefix:
        # List with specified prefix
        response = s3.list_objects_v2(Bucket=bucket, Prefix=prefix, MaxKeys=1000)
        all_objects.extend(response.get('Contents', []))
    else:
        # List root objects
        response = s3.list_objects_v2(Bucket=bucket, MaxKeys=1000)
        all_objects.extend(response.get('Contents', []))

        # Also list .vectors/ prefix (LeoFS stores these separately)
        response2 = s3.list_objects_v2(Bucket=bucket, Prefix='.vectors/', MaxKeys=1000)
        all_objects.extend(response2.get('Contents', []))

    if not all_objects:
        print(f'No objects found in s3://{bucket}/')
        sys.exit(0)

    # Sort: root objects first, then .vectors/
    for obj in sorted(all_objects, key=lambda x: (x['Key'].startswith('.'), x['Key'])):
        size = obj['Size']
        size_str = f'({size:,} bytes)'
        print(f'{obj[\"Key\"]}\t{size_str}')

except s3.exceptions.NoSuchBucket:
    print(f'Error: Bucket \"{bucket}\" does not exist')
    sys.exit(1)
except Exception as e:
    print(f'Error: {e}')
    sys.exit(1)
"
