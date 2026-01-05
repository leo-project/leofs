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
#   ./list-objects.sh bucket-6 .vectors/documents.lance/
#
# Note:
#   LeoFS list_objects_v2 may not return files with '.' prefixes.
#   For .vectors/documents.lance/, this script parses the LanceDB
#   manifest to discover and list all files.
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
    echo "  $0 bucket-6 .vectors/documents.lance/"
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
import re

s3 = boto3.client(
    's3',
    endpoint_url=os.environ.get('LEOFS_ENDPOINT', 'http://leogateway:8080'),
    aws_access_key_id='dummy',
    aws_secret_access_key='dummy',
    config=Config(signature_version='s3', s3={'addressing_style': 'path'})
)

bucket = '$BUCKET'
prefix = '$PREFIX'


def list_lancedb_files(bucket, table_prefix):
    '''
    List LanceDB files by parsing the manifest.
    LeoFS list_objects_v2 doesn't return files with '.' prefixes,
    so we need to read the manifest and probe files with head_object.
    '''
    files = []

    # Find the latest manifest version by probing
    latest_version = 0
    for v in range(1, 1000):
        key = f'{table_prefix}_versions/{v}.manifest'
        try:
            s3.head_object(Bucket=bucket, Key=key)
            latest_version = v
        except:
            break

    if latest_version == 0:
        return files

    # List all manifest files
    for v in range(1, latest_version + 1):
        key = f'{table_prefix}_versions/{v}.manifest'
        try:
            resp = s3.head_object(Bucket=bucket, Key=key)
            files.append({'Key': key, 'Size': resp['ContentLength']})
        except:
            pass

    # Read the latest manifest to find data files
    manifest_key = f'{table_prefix}_versions/{latest_version}.manifest'
    try:
        response = s3.get_object(Bucket=bucket, Key=manifest_key)
        manifest_data = response['Body'].read()
        text_data = manifest_data.decode('utf-8', errors='ignore')

        # Find .lance data file patterns (binary + hex UUID format)
        lance_files = set(re.findall(r'[01]{20,}[a-f0-9]{24,32}\\.lance', text_data))

        for fname in lance_files:
            key = f'{table_prefix}data/{fname}'
            try:
                resp = s3.head_object(Bucket=bucket, Key=key)
                files.append({'Key': key, 'Size': resp['ContentLength']})
            except:
                pass
    except Exception as e:
        print(f'Warning: Could not parse manifest: {e}', file=sys.stderr)

    return files


def list_vectors_prefix(bucket):
    '''
    List all LanceDB tables in .vectors/ prefix.
    '''
    files = []

    # Check for common table names
    table_names = ['documents']  # Add more if needed

    for table in table_names:
        table_prefix = f'.vectors/{table}.lance/'
        table_files = list_lancedb_files(bucket, table_prefix)
        files.extend(table_files)

    return files


try:
    all_objects = []
    used_lancedb_listing = False

    # Check if requesting .vectors/documents.lance/ specifically
    if prefix.startswith('.vectors/') and '.lance' in prefix:
        # Use LanceDB manifest-based listing
        table_prefix = prefix if prefix.endswith('/') else prefix + '/'
        all_objects = list_lancedb_files(bucket, table_prefix)
        used_lancedb_listing = True
    elif prefix == '.vectors/' or prefix == '.vectors':
        # List all LanceDB tables
        all_objects = list_vectors_prefix(bucket)
        used_lancedb_listing = True
    elif prefix:
        # List with specified prefix using S3 API
        response = s3.list_objects_v2(Bucket=bucket, Prefix=prefix, MaxKeys=1000)
        all_objects.extend(response.get('Contents', []))
    else:
        # List root objects
        response = s3.list_objects_v2(Bucket=bucket, MaxKeys=1000)
        all_objects.extend(response.get('Contents', []))

        # Also try to list LanceDB files (since list_objects_v2 won't find them)
        vectors_files = list_vectors_prefix(bucket)
        all_objects.extend(vectors_files)

    # Remove duplicates by key
    seen_keys = set()
    unique_objects = []
    for obj in all_objects:
        if obj['Key'] not in seen_keys:
            seen_keys.add(obj['Key'])
            unique_objects.append(obj)

    if not unique_objects:
        print(f'No objects found in s3://{bucket}/{prefix}')
        sys.exit(0)

    # Print header
    if used_lancedb_listing:
        print(f'# LanceDB files in s3://{bucket}/{prefix}')
        print(f'# (Listed via manifest parsing - list_objects_v2 does not return these)')
        print()

    # Sort and display
    for obj in sorted(unique_objects, key=lambda x: x['Key']):
        size = obj['Size']
        if size >= 1024 * 1024:
            size_str = f'{size / 1024 / 1024:.1f} MB'
        elif size >= 1024:
            size_str = f'{size / 1024:.1f} KB'
        else:
            size_str = f'{size:,} bytes'
        print(f'{obj[\"Key\"]:<70} {size_str:>12}')

    # Print summary
    total_size = sum(obj['Size'] for obj in unique_objects)
    print()
    print(f'Total: {len(unique_objects)} files, {total_size:,} bytes ({total_size/1024:.1f} KB)')

except Exception as e:
    import traceback
    print(f'Error: {e}')
    traceback.print_exc()
    sys.exit(1)
"
