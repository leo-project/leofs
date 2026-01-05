#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Batch upload test files to LeoFS and list objects.

Usage:
    python batch_upload.py -b BUCKET -a ACCESS_KEY -s SECRET_KEY [-n 100]
"""

import argparse
import re
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
            s3={"addressing_style": "path"},
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
            if i % 10 == 0 or i == file_count:
                print(f"  Uploaded: {i}/{file_count}")
        except ClientError as e:
            print(f"  ERROR uploading {key}: {e}")

    print("-" * 50)
    print(f"Successfully uploaded: {len(uploaded)} files")
    return uploaded


def list_objects_v1(s3, bucket: str, prefix: str = '', debug: bool = False):
    """List objects using S3 v1 API.

    Args:
        s3: boto3 S3 client
        bucket: Bucket name
        prefix: Optional prefix to filter objects
        debug: If True, print debug info

    Returns:
        List of (key, size) tuples
    """
    objects = []
    marker = ''

    while True:
        params = {'Bucket': bucket, 'MaxKeys': 1000}
        if prefix:
            params['Prefix'] = prefix
        if marker:
            params['Marker'] = marker

        if debug:
            print(f"  [DEBUG] list_objects v1 params: {params}")

        try:
            response = s3.list_objects(**params)

            if debug:
                print(f"  [DEBUG] IsTruncated: {response.get('IsTruncated')}")
                print(f"  [DEBUG] Contents count: {len(response.get('Contents', []))}")

            for obj in response.get('Contents', []):
                key = obj['Key']
                size = obj['Size']
                objects.append((key, size))
                if debug and len(objects) <= 10:
                    print(f"  [DEBUG]   Found: {key} ({size} bytes)")

            if response.get('IsTruncated'):
                marker = response.get('NextMarker') or objects[-1][0] if objects else ''
            else:
                break

        except ClientError as e:
            if debug:
                print(f"  [DEBUG] list_objects v1 error: {e}")
            raise

    return objects


def list_vectors_from_manifest(s3, bucket: str, lance_table: str = 'documents_vectors',
                               debug: bool = False):
    """List .vectors/ files by reading Lance manifest file.

    Lance stores metadata in _versions/*.manifest files that contain
    references to data files. This function:
    1. Fetches the latest manifest file
    2. Extracts data file references from the manifest
    3. Uses HEAD requests to get file sizes

    Args:
        s3: boto3 S3 client
        bucket: Bucket name
        lance_table: Lance table name (default: documents_vectors)
        debug: Enable debug output

    Returns:
        List of (key, size) tuples
    """
    vector_objects = []
    base_path = f".vectors/{lance_table}.lance"

    print(f"\n[4] Reading Lance manifest from {base_path}/_versions/...")

    # Find manifest files (1.manifest, 2.manifest, etc.)
    manifest_keys = []
    for version in range(1, 100):
        manifest_key = f"{base_path}/_versions/{version}.manifest"
        try:
            response = s3.head_object(Bucket=bucket, Key=manifest_key)
            size = response.get('ContentLength', 0)
            manifest_keys.append((manifest_key, size, version))
            if debug:
                print(f"  [DEBUG] Found manifest: {manifest_key} ({size} bytes)")
        except ClientError as e:
            if e.response['Error']['Code'] == '404':
                break
            break

    if not manifest_keys:
        print("    No manifest files found")
        return []

    print(f"    Found {len(manifest_keys)} manifest file(s)")

    # Add manifest files to the list
    for key, size, _ in manifest_keys:
        vector_objects.append((key, size))

    # Get the latest manifest content to extract data file references
    latest_manifest = manifest_keys[-1]
    manifest_content = None
    try:
        response = s3.get_object(Bucket=bucket, Key=latest_manifest[0])
        manifest_content = response['Body'].read()

        if debug:
            print(f"  [DEBUG] Manifest size: {len(manifest_content)} bytes")
            # Show printable strings in manifest for debugging
            import string
            printable = set(string.printable.encode())
            current_str = []
            strings_found = []
            for byte in manifest_content:
                if byte in printable and byte not in (ord('\n'), ord('\r'), ord('\t')):
                    current_str.append(chr(byte))
                else:
                    if len(current_str) >= 10:
                        strings_found.append(''.join(current_str))
                    current_str = []
            if current_str and len(current_str) >= 10:
                strings_found.append(''.join(current_str))
            print(f"  [DEBUG] Printable strings in manifest:")
            for s in strings_found[:20]:
                print(f"    {s}")

        # Extract .lance file references from manifest binary
        # Pattern: [0-1 binary digits]{20-30}[hex chars]{20-35}.lance
        # Example: 0110001110111000110110016569494bba88aaeaf0d37499b8.lance
        lance_files = set()
        pattern = rb'[01]{20,30}[0-9a-f]{20,35}\.lance'
        matches = re.findall(pattern, manifest_content)
        for match in matches:
            filename = match.decode('utf-8')
            lance_files.add(f"data/{filename}")

        if debug:
            print(f"  [DEBUG] Found {len(lance_files)} data file references in manifest")

        # HEAD request for each data file
        for data_file in sorted(lance_files):
            data_key = f"{base_path}/{data_file}"
            try:
                response = s3.head_object(Bucket=bucket, Key=data_key)
                size = response.get('ContentLength', 0)
                vector_objects.append((data_key, size))
                if debug:
                    print(f"  [DEBUG] Found data file: {data_key} ({size} bytes)")
            except ClientError as e:
                if debug:
                    print(f"  [DEBUG] HEAD failed for {data_key}: {e.response['Error']['Code']}")

    except ClientError as e:
        print(f"    Error reading manifest: {e}")

    # Find transaction files from manifest
    print(f"\n[5] Checking _transactions directory...")
    if manifest_content:
        try:
            txn_pattern = rb'[0-9]+-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\.txn'
            txn_matches = re.findall(txn_pattern, manifest_content)
            for match in txn_matches:
                txn_file = match.decode('utf-8')
                txn_key = f"{base_path}/_transactions/{txn_file}"
                try:
                    response = s3.head_object(Bucket=bucket, Key=txn_key)
                    size = response.get('ContentLength', 0)
                    vector_objects.append((txn_key, size))
                    if debug:
                        print(f"  [DEBUG] Found txn file: {txn_key} ({size} bytes)")
                except ClientError:
                    pass
        except Exception as e:
            if debug:
                print(f"  [DEBUG] Error finding txn files: {e}")

    print(f"    Found {len(vector_objects)} total .vectors/ files via manifest")
    return vector_objects


def list_objects(s3, bucket: str, debug: bool = False):
    """List all objects in bucket (regular + .vectors/) using v1 API."""
    print("\n" + "=" * 60)
    print("=== Uploaded Files (using list_objects v1 API) ===")
    print("=" * 60)

    regular_objects = []
    vector_objects = []

    try:
        # List all objects without prefix
        print("\n[1] Listing all objects (no prefix)...")
        all_objects = list_objects_v1(s3, bucket, prefix='', debug=debug)
        for key, size in all_objects:
            if key.startswith('.vectors/') or key.startswith('/.vectors/'):
                vector_objects.append((key, size))
            else:
                regular_objects.append((key, size))
        print(f"    Found {len(all_objects)} total objects")

        # List with .vectors/ prefix
        print("\n[2] Listing with '.vectors/' prefix...")
        vectors_list = list_objects_v1(s3, bucket, prefix='.vectors/', debug=debug)
        print(f"    Found {len(vectors_list)} objects with '.vectors/' prefix")
        for key, size in vectors_list:
            if (key, size) not in vector_objects:
                vector_objects.append((key, size))

        # Try with leading slash /.vectors/
        print("\n[3] Listing with '/.vectors/' prefix...")
        vectors_slash = list_objects_v1(s3, bucket, prefix='/.vectors/', debug=debug)
        print(f"    Found {len(vectors_slash)} objects with '/.vectors/' prefix")
        for key, size in vectors_slash:
            if (key, size) not in vector_objects:
                vector_objects.append((key, size))

        # If no .vectors/ found via list API, try manifest approach
        if not vector_objects:
            manifest_objects = list_vectors_from_manifest(s3, bucket, debug=debug)
            for key, size in manifest_objects:
                if (key, size) not in vector_objects:
                    vector_objects.append((key, size))

    except ClientError as e:
        print(f"ERROR listing objects: {e}")
        return

    # Print regular objects
    print("\n" + "-" * 60)
    print("Regular files:")
    print("-" * 60)
    for key, size in sorted(regular_objects):
        print(f"  {key}\t({size:,} bytes)")

    if not regular_objects:
        print("  (no regular files)")

    # Print .vectors/ objects
    print("\n" + "-" * 60)
    print(".vectors/ files (via manifest + HEAD):")
    print("-" * 60)
    for key, size in sorted(vector_objects):
        print(f"  {key}\t({size:,} bytes)")

    if not vector_objects:
        print("  (no .vectors/ files found)")

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
    parser.add_argument("--bucket", "-b", required=True, help="Bucket name")
    parser.add_argument("--access-key", "-a", required=True, help="Access key ID")
    parser.add_argument("--secret-key", "-s", required=True, help="Secret access key")
    parser.add_argument("--count", "-n", type=int, default=100,
                        help="Number of files to upload (default: 100)")
    parser.add_argument("--wait", "-w", type=int, default=5,
                        help="Wait time in seconds before listing (default: 5)")
    parser.add_argument("--debug", "-d", action="store_true",
                        help="Enable debug output for list_objects")
    parser.add_argument("--list-only", "-l", action="store_true",
                        help="Only list objects, skip upload")
    args = parser.parse_args()

    setup_expect_header_removal()

    print("=" * 60)
    print("LeoFS Batch Upload Test")
    print("=" * 60)
    print(f"  Endpoint:    {args.endpoint}")
    print(f"  Bucket:      {args.bucket}")
    print(f"  File count:  {args.count}")
    print(f"  Wait time:   {args.wait}s")
    print(f"  Debug:       {args.debug}")
    print(f"  List only:   {args.list_only}")
    print("=" * 60)
    print()

    s3 = create_s3_client(args.endpoint, args.access_key, args.secret_key)

    if not args.list_only:
        uploaded = upload_files(s3, args.bucket, args.count)
        if not uploaded:
            print("No files were uploaded. Exiting.")
            sys.exit(1)
        print(f"\nWaiting {args.wait} seconds...")
        time.sleep(args.wait)

    list_objects(s3, args.bucket, debug=args.debug)
    print("\nDone.")


if __name__ == "__main__":
    main()
