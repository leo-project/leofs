#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Batch upload test files to LeoFS and list objects.

Usage:
    python batch_upload.py -b BUCKET -a ACCESS_KEY -s SECRET_KEY [-n 100]
"""

import argparse
import os
import sys
import time

try:
    import boto3
    from botocore.config import Config
    from botocore.exceptions import ClientError
    import botocore.httpsession
except ImportError:
    print("ERROR: boto3 is required. Install with: pip install boto3")
    sys.exit(1)

try:
    import pyarrow.fs as pafs
    PYARROW_AVAILABLE = True
except ImportError:
    PYARROW_AVAILABLE = False


def setup_expect_header_removal():
    """Monkey-patch to remove Expect: 100-continue header from all requests."""
    _original_send = botocore.httpsession.URLLib3Session.send
    def _patched_send(self, request):
        if 'Expect' in request.headers:
            del request.headers['Expect']
        return _original_send(self, request)
    botocore.httpsession.URLLib3Session.send = _patched_send


def create_s3_client(endpoint: str, access_key: str, secret_key: str):
    """Create S3 client for LeoFS."""
    return boto3.client(
        "s3",
        endpoint_url=endpoint,
        aws_access_key_id=access_key,
        aws_secret_access_key=secret_key,
        region_name="us-east-1",
        config=Config(
            signature_version="s3",
            s3={
                "addressing_style": "path",
            },
            retries={"max_attempts": 1},
        ),
    )


def upload_files(s3, bucket: str, file_count: int) -> list:
    """Upload test files and return list of uploaded keys."""
    uploaded = []
    print(f"Uploading {file_count} files to s3://{bucket}/")
    print("-" * 50)

    for i in range(1, file_count + 1):
        key = f"file_{i:04d}.txt"
        content = f"Test file {i:04d} - batch upload test content\n"

        try:
            s3.put_object(
                Bucket=bucket,
                Key=key,
                Body=content.encode('utf-8'),
                ContentType="text/plain"
            )
            uploaded.append(key)
            # Progress indicator every 10 files
            if i % 10 == 0 or i == file_count:
                print(f"  Uploaded: {i}/{file_count}")
        except ClientError as e:
            print(f"  ERROR uploading {key}: {e}")

    print("-" * 50)
    print(f"Successfully uploaded: {len(uploaded)} files")
    return uploaded


def list_vectors_pyarrow(endpoint: str, access_key: str, secret_key: str, bucket: str):
    """List .vectors/ files using PyArrow filesystem (more reliable for LeoFS)."""
    if not PYARROW_AVAILABLE:
        return []

    try:
        # Parse endpoint to get host and port
        from urllib.parse import urlparse
        parsed = urlparse(endpoint)
        host_port = parsed.netloc  # e.g., "127.0.0.1:28080"

        fs = pafs.S3FileSystem(
            endpoint_override=host_port,
            access_key=access_key,
            secret_key=secret_key,
            scheme=parsed.scheme,
            region="us-east-1",
        )

        vector_objects = []
        lance_dirs = {}  # Track .lance directories with their stats

        try:
            # First, list .vectors directory to find .lance directories
            files = fs.get_file_info(pafs.FileSelector(f"{bucket}/.vectors", recursive=False))
            for f in files:
                if f.type == pafs.FileType.Directory and f.path.endswith('.lance'):
                    lance_path = f.path
                    lance_key = f.path.replace(f"{bucket}/", "", 1)

                    # List data directory inside .lance to get actual files
                    total_size = 0
                    file_count = 0
                    try:
                        data_files = fs.get_file_info(
                            pafs.FileSelector(f"{lance_path}/data", recursive=True)
                        )
                        for df in data_files:
                            if df.type == pafs.FileType.File and df.size:
                                total_size += df.size
                                file_count += 1
                    except Exception:
                        pass  # data directory may not exist yet

                    lance_dirs[lance_key] = {
                        'size': total_size,
                        'files': file_count
                    }

            # Create summary entries for each .lance directory
            for lance_key, stats in lance_dirs.items():
                summary = f"{lance_key}/ [{stats['files']} files]"
                vector_objects.append((summary, stats['size']))

        except Exception as e:
            print(f"  (PyArrow .vectors listing error: {e})")

        return vector_objects
    except Exception as e:
        print(f"  (PyArrow listing failed: {e})")
        return []


def list_objects(s3, bucket: str, endpoint: str = None, access_key: str = None, secret_key: str = None):
    """List all objects in bucket (regular + .vectors/)."""
    print("\n" + "=" * 60)
    print("=== Uploaded Files ===")
    print("=" * 60)

    regular_objects = []
    vector_objects = []

    try:
        # List regular objects (exclude .vectors/)
        paginator = s3.get_paginator('list_objects_v2')
        for page in paginator.paginate(Bucket=bucket, MaxKeys=1000):
            for obj in page.get('Contents', []):
                key = obj['Key']
                size = obj['Size']
                if key.startswith('.vectors/') or key.startswith('/.vectors/'):
                    vector_objects.append((key, size))
                else:
                    regular_objects.append((key, size))

        # Try boto3 prefix listing first
        for page in paginator.paginate(Bucket=bucket, Prefix='.vectors/', MaxKeys=1000):
            for obj in page.get('Contents', []):
                key = obj['Key']
                size = obj['Size']
                if (key, size) not in vector_objects:
                    vector_objects.append((key, size))

    except ClientError as e:
        print(f"ERROR listing objects: {e}")
        return

    # If no vectors found via boto3, try PyArrow (works better with LeoFS)
    if not vector_objects and endpoint and access_key and secret_key:
        if PYARROW_AVAILABLE:
            print("  (Using PyArrow to list .vectors/ files...)")
            vector_objects = list_vectors_pyarrow(endpoint, access_key, secret_key, bucket)
        else:
            print("  (PyArrow not available - install with: pip install pyarrow)")

    # Print regular objects
    for key, size in sorted(regular_objects):
        print(f"  {key}\t({size:,} bytes)")

    if not regular_objects:
        print("  (no regular files)")

    # Print .vectors/ objects
    print("\n" + "=" * 60)
    print("=== .vectors/ Files ===")
    print("=" * 60)

    for key, size in sorted(vector_objects):
        print(f"  {key}\t({size:,} bytes)")

    if not vector_objects:
        print("  (no .vectors/ files)")

    # Summary
    print("\n" + "=" * 60)
    print("=== Summary ===")
    print("=" * 60)
    print(f"  Regular files:  {len(regular_objects)}")
    print(f"  .vectors files: {len(vector_objects)}")
    print(f"  Total:          {len(regular_objects) + len(vector_objects)}")


def main():
    parser = argparse.ArgumentParser(description="Batch upload test files to LeoFS")
    parser.add_argument("--endpoint", "-e", default="http://127.0.0.1:28080",
                        help="LeoFS endpoint URL")
    parser.add_argument("--bucket", "-b", required=True,
                        help="Bucket name")
    parser.add_argument("--access-key", "-a", required=True,
                        help="Access key ID")
    parser.add_argument("--secret-key", "-s", required=True,
                        help="Secret access key")
    parser.add_argument("--count", "-n", type=int, default=100,
                        help="Number of files to upload (default: 100)")
    parser.add_argument("--wait", "-w", type=int, default=5,
                        help="Wait time in seconds before listing (default: 5)")
    args = parser.parse_args()

    # Setup
    setup_expect_header_removal()

    print("=" * 60)
    print("LeoFS Batch Upload Test")
    print("=" * 60)
    print(f"  Endpoint:    {args.endpoint}")
    print(f"  Bucket:      {args.bucket}")
    print(f"  File count:  {args.count}")
    print(f"  Wait time:   {args.wait}s")
    print("=" * 60)
    print()

    # Create S3 client
    s3 = create_s3_client(args.endpoint, args.access_key, args.secret_key)

    # Upload files
    uploaded = upload_files(s3, args.bucket, args.count)

    if not uploaded:
        print("No files were uploaded. Exiting.")
        sys.exit(1)

    # Wait
    print(f"\nWaiting {args.wait} seconds...")
    time.sleep(args.wait)

    # List objects
    list_objects(s3, args.bucket, args.endpoint, args.access_key, args.secret_key)

    print("\nDone.")


if __name__ == "__main__":
    main()
