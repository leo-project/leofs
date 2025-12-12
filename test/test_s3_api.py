#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
LeoFS S3 API Integration Test

This test suite verifies the S3 API compatibility of leo_gateway
using Python boto3 library.

Test operations:
- Bucket creation
- File upload (PUT object)
- File retrieval (GET object)
- File deletion (DELETE object)
- Bucket deletion

Usage:
    python test_s3_api.py [--endpoint URL] [--access-key KEY] [--secret-key KEY]

Environment variables (alternative to command line args):
    LEOFS_ENDPOINT      - S3 endpoint URL (default: http://localhost:8080)
    LEOFS_ACCESS_KEY    - Access key ID
    LEOFS_SECRET_KEY    - Secret access key
"""

import argparse
import hashlib
import os
import sys
import time
import unittest
import uuid

import boto3
from botocore.config import Config
from botocore.exceptions import ClientError


class LeoFSS3TestConfig:
    """Configuration for LeoFS S3 API tests."""

    def __init__(self, endpoint=None, access_key=None, secret_key=None):
        self.endpoint = endpoint or os.environ.get('LEOFS_ENDPOINT', 'http://localhost:8080')
        self.access_key = access_key or os.environ.get('LEOFS_ACCESS_KEY', 'YOUR_ACCESS_KEY')
        self.secret_key = secret_key or os.environ.get('LEOFS_SECRET_KEY', 'YOUR_SECRET_KEY')
        self.region = 'us-east-1'


class LeoFSS3APITest(unittest.TestCase):
    """Test suite for LeoFS S3 API operations."""

    config = None

    @classmethod
    def setUpClass(cls):
        """Set up test fixtures."""
        if cls.config is None:
            cls.config = LeoFSS3TestConfig()

        # Create S3 client with path-style addressing (required for LeoFS)
        # LeoFS supports AWS Signature V2, not V4
        cls.s3_client = boto3.client(
            's3',
            endpoint_url=cls.config.endpoint,
            aws_access_key_id=cls.config.access_key,
            aws_secret_access_key=cls.config.secret_key,
            region_name=cls.config.region,
            config=Config(
                s3={'addressing_style': 'path'},
                signature_version='s3',
                retries={'max_attempts': 3}
            )
        )

        # Generate unique test bucket name
        cls.test_bucket = f'test-bucket-{uuid.uuid4().hex[:8]}'
        cls.test_objects = []

    @classmethod
    def tearDownClass(cls):
        """Clean up test resources."""
        try:
            # Delete all test objects
            for obj_key in cls.test_objects:
                try:
                    cls.s3_client.delete_object(Bucket=cls.test_bucket, Key=obj_key)
                except ClientError:
                    pass

            # Delete test bucket
            try:
                cls.s3_client.delete_bucket(Bucket=cls.test_bucket)
            except ClientError:
                pass
        except Exception as e:
            print(f"Warning: Cleanup failed: {e}", file=sys.stderr)

    def test_01_create_bucket(self):
        """Test bucket creation."""
        print(f"\n[Test] Creating bucket: {self.test_bucket}")

        try:
            response = self.s3_client.create_bucket(Bucket=self.test_bucket)
            self.assertIn('Location', response)
            print(f"  -> Bucket created successfully. Location: {response.get('Location', '/')}")
        except ClientError as e:
            error_code = e.response['Error']['Code']
            if error_code == 'BucketAlreadyOwnedByYou':
                print(f"  -> Bucket already exists (owned by you)")
            else:
                raise

    def test_02_list_buckets(self):
        """Test listing buckets."""
        print("\n[Test] Listing buckets")

        response = self.s3_client.list_buckets()
        buckets = response.get('Buckets', [])
        bucket_names = [b['Name'] for b in buckets]

        print(f"  -> Found {len(buckets)} bucket(s): {bucket_names}")
        self.assertIn(self.test_bucket, bucket_names,
                     f"Test bucket '{self.test_bucket}' not found in bucket list")

    def test_03_put_object_small(self):
        """Test uploading a small object."""
        object_key = 'test-small-object.txt'
        object_content = b'Hello, LeoFS! This is a small test file.'

        print(f"\n[Test] Uploading small object: {object_key}")
        print(f"  -> Content size: {len(object_content)} bytes")

        response = self.s3_client.put_object(
            Bucket=self.test_bucket,
            Key=object_key,
            Body=object_content,
            ContentType='text/plain'
        )

        self.__class__.test_objects.append(object_key)

        self.assertIn('ETag', response)
        print(f"  -> Upload successful. ETag: {response['ETag']}")

    def test_04_put_object_binary(self):
        """Test uploading a binary object."""
        object_key = 'test-binary-object.bin'
        # Generate random binary content
        object_content = os.urandom(1024)  # 1KB random data

        print(f"\n[Test] Uploading binary object: {object_key}")
        print(f"  -> Content size: {len(object_content)} bytes")

        # Calculate MD5 for verification
        content_md5 = hashlib.md5(object_content).hexdigest()

        response = self.s3_client.put_object(
            Bucket=self.test_bucket,
            Key=object_key,
            Body=object_content,
            ContentType='application/octet-stream'
        )

        self.__class__.test_objects.append(object_key)

        self.assertIn('ETag', response)
        print(f"  -> Upload successful. ETag: {response['ETag']}")
        print(f"  -> Content MD5: {content_md5}")

    def test_05_put_object_with_path(self):
        """Test uploading an object with directory-like path."""
        object_key = 'path/to/nested/test-file.txt'
        object_content = b'This is a nested file in a directory-like path.'

        print(f"\n[Test] Uploading object with path: {object_key}")
        print(f"  -> Content size: {len(object_content)} bytes")

        response = self.s3_client.put_object(
            Bucket=self.test_bucket,
            Key=object_key,
            Body=object_content,
            ContentType='text/plain'
        )

        self.__class__.test_objects.append(object_key)

        self.assertIn('ETag', response)
        print(f"  -> Upload successful. ETag: {response['ETag']}")

    def test_06_get_object_small(self):
        """Test retrieving a small object."""
        object_key = 'test-small-object.txt'
        expected_content = b'Hello, LeoFS! This is a small test file.'

        print(f"\n[Test] Retrieving small object: {object_key}")

        response = self.s3_client.get_object(
            Bucket=self.test_bucket,
            Key=object_key
        )

        actual_content = response['Body'].read()

        self.assertEqual(actual_content, expected_content)
        self.assertEqual(response['ContentType'], 'text/plain')
        print(f"  -> Retrieved successfully. Size: {len(actual_content)} bytes")
        print(f"  -> Content-Type: {response['ContentType']}")
        print(f"  -> Content matches expected: True")

    def test_07_get_object_with_path(self):
        """Test retrieving an object with directory-like path."""
        object_key = 'path/to/nested/test-file.txt'
        expected_content = b'This is a nested file in a directory-like path.'

        print(f"\n[Test] Retrieving object with path: {object_key}")

        response = self.s3_client.get_object(
            Bucket=self.test_bucket,
            Key=object_key
        )

        actual_content = response['Body'].read()

        self.assertEqual(actual_content, expected_content)
        print(f"  -> Retrieved successfully. Size: {len(actual_content)} bytes")
        print(f"  -> Content matches expected: True")

    def test_08_head_object(self):
        """Test HEAD object request (get metadata without body)."""
        object_key = 'test-small-object.txt'

        print(f"\n[Test] HEAD request for object: {object_key}")

        response = self.s3_client.head_object(
            Bucket=self.test_bucket,
            Key=object_key
        )

        self.assertIn('ContentLength', response)
        self.assertIn('ETag', response)
        self.assertIn('ContentType', response)

        print(f"  -> Content-Length: {response['ContentLength']}")
        print(f"  -> Content-Type: {response['ContentType']}")
        print(f"  -> ETag: {response['ETag']}")

    def test_09_list_objects(self):
        """Test listing objects in bucket."""
        print(f"\n[Test] Listing objects in bucket: {self.test_bucket}")

        response = self.s3_client.list_objects_v2(Bucket=self.test_bucket)

        objects = response.get('Contents', [])
        object_keys = [obj['Key'] for obj in objects]

        print(f"  -> Found {len(objects)} object(s)")
        for obj in objects:
            print(f"     - {obj['Key']} ({obj['Size']} bytes)")

        # Verify our test objects are in the list
        self.assertIn('test-small-object.txt', object_keys)
        self.assertIn('test-binary-object.bin', object_keys)
        self.assertIn('path/to/nested/test-file.txt', object_keys)

    def test_10_list_objects_with_prefix(self):
        """Test listing objects with prefix filter."""
        prefix = 'path/to/'

        print(f"\n[Test] Listing objects with prefix: {prefix}")

        response = self.s3_client.list_objects_v2(
            Bucket=self.test_bucket,
            Prefix=prefix
        )

        objects = response.get('Contents', [])

        print(f"  -> Found {len(objects)} object(s) with prefix '{prefix}'")
        for obj in objects:
            print(f"     - {obj['Key']} ({obj['Size']} bytes)")
            self.assertTrue(obj['Key'].startswith(prefix))

    def test_11_copy_object(self):
        """Test copying an object within the same bucket."""
        source_key = 'test-small-object.txt'
        dest_key = 'test-small-object-copy.txt'

        print(f"\n[Test] Copying object: {source_key} -> {dest_key}")

        response = self.s3_client.copy_object(
            Bucket=self.test_bucket,
            CopySource=f'{self.test_bucket}/{source_key}',
            Key=dest_key
        )

        self.__class__.test_objects.append(dest_key)

        self.assertIn('CopyObjectResult', response)
        print(f"  -> Copy successful. ETag: {response['CopyObjectResult']['ETag']}")

        # Verify the copied object
        get_response = self.s3_client.get_object(
            Bucket=self.test_bucket,
            Key=dest_key
        )
        copied_content = get_response['Body'].read()
        expected_content = b'Hello, LeoFS! This is a small test file.'

        self.assertEqual(copied_content, expected_content)
        print(f"  -> Copied content verified successfully")

    def test_12_delete_object(self):
        """Test deleting a single object."""
        object_key = 'test-small-object-copy.txt'

        print(f"\n[Test] Deleting object: {object_key}")

        # Delete the object
        response = self.s3_client.delete_object(
            Bucket=self.test_bucket,
            Key=object_key
        )

        if object_key in self.__class__.test_objects:
            self.__class__.test_objects.remove(object_key)

        print(f"  -> Delete request successful")

        # Verify object is deleted
        with self.assertRaises(ClientError) as context:
            self.s3_client.get_object(
                Bucket=self.test_bucket,
                Key=object_key
            )

        error_code = context.exception.response['Error']['Code']
        self.assertIn(error_code, ['NoSuchKey', '404'])
        print(f"  -> Verified object no longer exists (Error: {error_code})")

    def test_13_delete_multiple_objects(self):
        """Test deleting multiple objects at once."""
        # First, create some objects to delete
        objects_to_delete = []
        for i in range(3):
            key = f'bulk-delete-test-{i}.txt'
            self.s3_client.put_object(
                Bucket=self.test_bucket,
                Key=key,
                Body=f'Bulk delete test content {i}'.encode()
            )
            objects_to_delete.append({'Key': key})

        print(f"\n[Test] Deleting multiple objects: {[o['Key'] for o in objects_to_delete]}")

        response = self.s3_client.delete_objects(
            Bucket=self.test_bucket,
            Delete={'Objects': objects_to_delete}
        )

        deleted = response.get('Deleted', [])
        print(f"  -> Deleted {len(deleted)} object(s)")

        self.assertEqual(len(deleted), 3)

        # Verify all objects are deleted
        for obj in objects_to_delete:
            with self.assertRaises(ClientError):
                self.s3_client.get_object(
                    Bucket=self.test_bucket,
                    Key=obj['Key']
                )
        print(f"  -> Verified all objects no longer exist")

    def test_14_get_nonexistent_object(self):
        """Test error handling for non-existent object."""
        object_key = 'nonexistent-object-12345.txt'

        print(f"\n[Test] Getting non-existent object: {object_key}")

        with self.assertRaises(ClientError) as context:
            self.s3_client.get_object(
                Bucket=self.test_bucket,
                Key=object_key
            )

        error_code = context.exception.response['Error']['Code']
        self.assertIn(error_code, ['NoSuchKey', '404'])
        print(f"  -> Correctly received error: {error_code}")

    def test_15_cleanup_and_delete_bucket(self):
        """Test cleanup: delete all objects and bucket."""
        print(f"\n[Test] Cleaning up test resources")

        # Delete all remaining test objects
        for obj_key in list(self.__class__.test_objects):
            try:
                self.s3_client.delete_object(
                    Bucket=self.test_bucket,
                    Key=obj_key
                )
                print(f"  -> Deleted object: {obj_key}")
                self.__class__.test_objects.remove(obj_key)
            except ClientError as e:
                print(f"  -> Warning: Failed to delete {obj_key}: {e}")

        # Delete the bucket
        print(f"\n[Test] Deleting bucket: {self.test_bucket}")

        try:
            self.s3_client.delete_bucket(Bucket=self.test_bucket)
            print(f"  -> Bucket deleted successfully")
        except ClientError as e:
            error_code = e.response['Error']['Code']
            if error_code == 'BucketNotEmpty':
                # List and delete any remaining objects
                response = self.s3_client.list_objects_v2(Bucket=self.test_bucket)
                for obj in response.get('Contents', []):
                    self.s3_client.delete_object(
                        Bucket=self.test_bucket,
                        Key=obj['Key']
                    )
                self.s3_client.delete_bucket(Bucket=self.test_bucket)
                print(f"  -> Bucket deleted after removing remaining objects")
            else:
                raise

        # Verify bucket is deleted
        time.sleep(1)  # Brief delay for consistency
        response = self.s3_client.list_buckets()
        bucket_names = [b['Name'] for b in response.get('Buckets', [])]

        self.assertNotIn(self.test_bucket, bucket_names)
        print(f"  -> Verified bucket no longer exists")


class LargeFIleTest(unittest.TestCase):
    """Test suite for large file operations (optional)."""

    config = None

    @classmethod
    def setUpClass(cls):
        """Set up test fixtures."""
        if cls.config is None:
            cls.config = LeoFSS3TestConfig()

        cls.s3_client = boto3.client(
            's3',
            endpoint_url=cls.config.endpoint,
            aws_access_key_id=cls.config.access_key,
            aws_secret_access_key=cls.config.secret_key,
            region_name=cls.config.region,
            config=Config(
                s3={'addressing_style': 'path'},
                signature_version='s3'
            )
        )

        cls.test_bucket = f'test-large-{uuid.uuid4().hex[:8]}'

        # Create bucket for large file tests
        try:
            cls.s3_client.create_bucket(Bucket=cls.test_bucket)
        except ClientError as e:
            if e.response['Error']['Code'] != 'BucketAlreadyOwnedByYou':
                raise

    @classmethod
    def tearDownClass(cls):
        """Clean up test resources."""
        try:
            # List and delete all objects
            response = cls.s3_client.list_objects_v2(Bucket=cls.test_bucket)
            for obj in response.get('Contents', []):
                cls.s3_client.delete_object(
                    Bucket=cls.test_bucket,
                    Key=obj['Key']
                )
            cls.s3_client.delete_bucket(Bucket=cls.test_bucket)
        except Exception:
            pass

    def test_large_file_upload_download(self):
        """Test uploading and downloading a larger file (1MB)."""
        object_key = 'large-test-file.bin'
        file_size = 1024 * 1024  # 1MB
        object_content = os.urandom(file_size)
        content_md5 = hashlib.md5(object_content).hexdigest()

        print(f"\n[Test] Large file upload/download: {object_key}")
        print(f"  -> File size: {file_size / 1024:.0f} KB")

        # Upload
        start_time = time.time()
        self.s3_client.put_object(
            Bucket=self.test_bucket,
            Key=object_key,
            Body=object_content
        )
        upload_time = time.time() - start_time
        print(f"  -> Upload completed in {upload_time:.2f}s")

        # Download
        start_time = time.time()
        response = self.s3_client.get_object(
            Bucket=self.test_bucket,
            Key=object_key
        )
        downloaded_content = response['Body'].read()
        download_time = time.time() - start_time
        print(f"  -> Download completed in {download_time:.2f}s")

        # Verify
        downloaded_md5 = hashlib.md5(downloaded_content).hexdigest()
        self.assertEqual(content_md5, downloaded_md5)
        print(f"  -> MD5 verification passed: {content_md5}")


def run_tests(config):
    """Run all test suites."""
    # Set config for test classes
    LeoFSS3APITest.config = config
    LargeFIleTest.config = config

    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    # Add main API tests
    suite.addTests(loader.loadTestsFromTestCase(LeoFSS3APITest))

    # Add large file tests
    suite.addTests(loader.loadTestsFromTestCase(LargeFIleTest))

    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    return 0 if result.wasSuccessful() else 1


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description='LeoFS S3 API Integration Test',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog='''
Examples:
    # Run with default settings (localhost:8080)
    python test_s3_api.py

    # Run with custom endpoint and credentials
    python test_s3_api.py --endpoint http://192.168.1.100:8080 \\
                          --access-key mykey --secret-key mysecret

    # Use environment variables
    export LEOFS_ENDPOINT=http://localhost:8080
    export LEOFS_ACCESS_KEY=mykey
    export LEOFS_SECRET_KEY=mysecret
    python test_s3_api.py
        '''
    )

    parser.add_argument(
        '--endpoint', '-e',
        help='S3 endpoint URL (default: http://localhost:8080)'
    )
    parser.add_argument(
        '--access-key', '-a',
        help='AWS access key ID'
    )
    parser.add_argument(
        '--secret-key', '-s',
        help='AWS secret access key'
    )

    args = parser.parse_args()

    # Create configuration
    config = LeoFSS3TestConfig(
        endpoint=args.endpoint,
        access_key=args.access_key,
        secret_key=args.secret_key
    )

    print("=" * 60)
    print("LeoFS S3 API Integration Test")
    print("=" * 60)
    print(f"Endpoint:   {config.endpoint}")
    print(f"Access Key: {config.access_key[:4]}..." if config.access_key else "Access Key: (not set)")
    print("=" * 60)

    return run_tests(config)


if __name__ == '__main__':
    sys.exit(main())
