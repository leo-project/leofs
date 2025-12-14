#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Debug boto3 requests to see exactly what's being sent.

Usage:
    python test_s3_boto3_debug.py -b BUCKET -a ACCESS_KEY -s SECRET_KEY [--no-expect]

Options:
    --no-expect: Remove Expect: 100-continue header (workaround for LeoFS)
    --no-debug: Disable verbose debug output
"""

import argparse
import logging
import os
import sys

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
    print("INFO: Expect header removal enabled")


def main():
    parser = argparse.ArgumentParser(description="Debug boto3 S3 requests")
    parser.add_argument("--endpoint", "-e", default="http://127.0.0.1:8080")
    parser.add_argument("--bucket", "-b", required=True)
    parser.add_argument("--access-key", "-a", default=os.environ.get("AWS_ACCESS_KEY_ID", ""))
    parser.add_argument("--secret-key", "-s", default=os.environ.get("AWS_SECRET_ACCESS_KEY", ""))
    parser.add_argument("--sig-version", choices=["s3", "s3v4"], default="s3",
                        help="Signature version: s3 (v2) or s3v4 (v4)")
    parser.add_argument("--no-expect", action="store_true",
                        help="Remove Expect: 100-continue header (workaround for LeoFS)")
    parser.add_argument("--no-debug", action="store_true",
                        help="Disable verbose debug output")
    args = parser.parse_args()

    if not args.access_key or not args.secret_key:
        parser.error("Access key and secret key are required")

    # Setup Expect header removal if requested
    if args.no_expect:
        setup_expect_header_removal()

    # Enable debugging unless --no-debug is specified
    if not args.no_debug:
        boto3.set_stream_logger('', logging.DEBUG)
        logging.getLogger('botocore').setLevel(logging.DEBUG)
        logging.getLogger('urllib3').setLevel(logging.DEBUG)

    print("="*70)
    print("boto3 Request Debug")
    print("="*70)
    print(f"Endpoint:    {args.endpoint}")
    print(f"Bucket:      {args.bucket}")
    print(f"Sig Version: {args.sig_version}")
    print(f"No Expect:   {args.no_expect}")
    print("="*70)

    s3 = boto3.client(
        "s3",
        endpoint_url=args.endpoint,
        aws_access_key_id=args.access_key,
        aws_secret_access_key=args.secret_key,
        region_name="us-east-1",
        config=Config(
            signature_version=args.sig_version,
            s3={
                "addressing_style": "path",
                "payload_signing_enabled": False,  # Disable payload signing
            },
            retries={"max_attempts": 1},
            request_checksum_calculation="when_required",  # Disable auto checksum
            response_checksum_validation="when_required",
        ),
    )

    test_key = "boto3-test.txt"
    test_data = b"Hello from boto3 - test content!"
    results = []

    # Test 1: List Buckets
    print("\n" + "="*70)
    print("TEST 1: List Buckets")
    print("="*70)
    try:
        response = s3.list_buckets()
        print(f"SUCCESS: Found {len(response.get('Buckets', []))} buckets")
        results.append(("List Buckets", "PASS"))
    except ClientError as e:
        print(f"FAILED: {e}")
        results.append(("List Buckets", "FAIL"))

    # Test 2: Head Bucket
    print("\n" + "="*70)
    print("TEST 2: Head Bucket")
    print("="*70)
    try:
        response = s3.head_bucket(Bucket=args.bucket)
        print(f"SUCCESS")
        results.append(("Head Bucket", "PASS"))
    except ClientError as e:
        print(f"FAILED: {e}")
        results.append(("Head Bucket", "FAIL"))

    # Test 3: Put Object
    print("\n" + "="*70)
    print("TEST 3: Put Object")
    print("="*70)
    try:
        response = s3.put_object(
            Bucket=args.bucket,
            Key=test_key,
            Body=test_data,
            ContentType="text/plain"
        )
        print(f"SUCCESS: ETag={response.get('ETag')}")
        results.append(("Put Object", "PASS"))
    except ClientError as e:
        print(f"FAILED: {e}")
        results.append(("Put Object", "FAIL"))

    # Test 4: Get Object
    print("\n" + "="*70)
    print("TEST 4: Get Object")
    print("="*70)
    try:
        response = s3.get_object(Bucket=args.bucket, Key=test_key)
        body = response['Body'].read()
        if body == test_data:
            print(f"SUCCESS: Content verified ({len(body)} bytes)")
            results.append(("Get Object", "PASS"))
        else:
            print(f"FAILED: Content mismatch (got {len(body)} bytes)")
            results.append(("Get Object", "FAIL"))
    except ClientError as e:
        print(f"FAILED: {e}")
        results.append(("Get Object", "FAIL"))

    # Test 5: Head Object
    print("\n" + "="*70)
    print("TEST 5: Head Object")
    print("="*70)
    try:
        response = s3.head_object(Bucket=args.bucket, Key=test_key)
        print(f"SUCCESS: Content-Length={response.get('ContentLength')}")
        results.append(("Head Object", "PASS"))
    except ClientError as e:
        print(f"FAILED: {e}")
        results.append(("Head Object", "FAIL"))

    # Test 6: List Objects (v1)
    print("\n" + "="*70)
    print("TEST 6: List Objects (v1)")
    print("="*70)
    try:
        response = s3.list_objects(Bucket=args.bucket, MaxKeys=10)
        print(f"SUCCESS: Found {len(response.get('Contents', []))} objects")
        results.append(("List Objects v1", "PASS"))
    except ClientError as e:
        print(f"FAILED: {e}")
        results.append(("List Objects v1", "FAIL"))

    # Test 7: List Objects (v2)
    print("\n" + "="*70)
    print("TEST 7: List Objects (v2)")
    print("="*70)
    try:
        response = s3.list_objects_v2(Bucket=args.bucket, MaxKeys=10)
        print(f"SUCCESS: Found {len(response.get('Contents', []))} objects")
        results.append(("List Objects v2", "PASS"))
    except ClientError as e:
        print(f"FAILED: {e}")
        results.append(("List Objects v2", "FAIL"))

    # Test 8: Delete Object
    print("\n" + "="*70)
    print("TEST 8: Delete Object")
    print("="*70)
    try:
        s3.delete_object(Bucket=args.bucket, Key=test_key)
        print(f"SUCCESS")
        results.append(("Delete Object", "PASS"))
    except ClientError as e:
        print(f"FAILED: {e}")
        results.append(("Delete Object", "FAIL"))

    # Test 9: Verify Delete (HEAD should return 404)
    print("\n" + "="*70)
    print("TEST 9: Verify Delete")
    print("="*70)
    try:
        s3.head_object(Bucket=args.bucket, Key=test_key)
        print(f"FAILED: Object still exists")
        results.append(("Verify Delete", "FAIL"))
    except ClientError as e:
        if e.response['Error']['Code'] == '404':
            print(f"SUCCESS: Object deleted (404 Not Found)")
            results.append(("Verify Delete", "PASS"))
        else:
            print(f"FAILED: {e}")
            results.append(("Verify Delete", "FAIL"))

    # Summary
    print("\n" + "="*70)
    print("SUMMARY")
    print("="*70)
    passed = sum(1 for _, r in results if r == "PASS")
    failed = sum(1 for _, r in results if r == "FAIL")
    for name, result in results:
        status = "✓" if result == "PASS" else "✗"
        print(f"  {status} {name}: {result}")
    print("-"*70)
    print(f"  Total: {passed} passed, {failed} failed")
    print("="*70)


if __name__ == "__main__":
    main()
