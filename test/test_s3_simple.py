#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
LeoFS S3 API Simple Test

A lightweight S3 API test using manual AWS Signature V2 calculation.
This ensures compatibility with LeoFS's S3 signature verification.

Usage:
    python test_s3_simple.py --bucket BUCKET --access-key KEY --secret-key KEY

Requirements:
    No external dependencies (uses only Python standard library)
"""

import argparse
import base64
import datetime
import hashlib
import hmac
import os
import sys
import urllib.request
import urllib.error
import xml.etree.ElementTree as ET


class LeoFSS3Client:
    """Simple S3 client with manual AWS Signature V2."""

    def __init__(self, endpoint, access_key, secret_key):
        self.endpoint = endpoint.rstrip("/")
        self.access_key = access_key
        self.secret_key = secret_key

    def _get_date(self):
        """Get current date in HTTP format."""
        return datetime.datetime.now(datetime.UTC).strftime("%a, %d %b %Y %H:%M:%S GMT")

    def _sign(self, method, resource, content_type="", content_md5="", date=None):
        """Calculate AWS Signature V2."""
        if date is None:
            date = self._get_date()

        string_to_sign = f"{method}\n{content_md5}\n{content_type}\n{date}\n{resource}"
        signature = base64.b64encode(
            hmac.new(
                self.secret_key.encode(), string_to_sign.encode(), hashlib.sha1
            ).digest()
        ).decode()

        return date, f"AWS {self.access_key}:{signature}"

    def _request(self, method, path, body=None, content_type="", headers=None):
        """Make HTTP request with AWS signature."""
        resource = path
        content_md5 = ""

        if body and method == "PUT":
            content_md5 = base64.b64encode(hashlib.md5(body).digest()).decode()

        date, auth = self._sign(method, resource, content_type, content_md5)

        url = f"{self.endpoint}{path}"
        req_headers = {"Date": date, "Authorization": auth}

        if content_type:
            req_headers["Content-Type"] = content_type
        if content_md5:
            req_headers["Content-MD5"] = content_md5
        if headers:
            req_headers.update(headers)

        req = urllib.request.Request(url, data=body, headers=req_headers, method=method)

        try:
            response = urllib.request.urlopen(req, timeout=30)
            return response
        except urllib.error.HTTPError as e:
            raise e

    def put_object(self, bucket, key, body, content_type="application/octet-stream"):
        """Upload an object to S3."""
        response = self._request("PUT", f"/{bucket}/{key}", body, content_type)
        return {"status": response.status, "etag": response.headers.get("ETag", "")}

    def get_object(self, bucket, key):
        """Download an object from S3."""
        response = self._request("GET", f"/{bucket}/{key}")
        return {
            "body": response.read(),
            "content_type": response.headers.get("Content-Type", ""),
            "content_length": int(response.headers.get("Content-Length", 0)),
            "etag": response.headers.get("ETag", ""),
        }

    def head_object(self, bucket, key):
        """Get object metadata."""
        response = self._request("HEAD", f"/{bucket}/{key}")
        return {
            "content_type": response.headers.get("Content-Type", ""),
            "content_length": int(response.headers.get("Content-Length", 0)),
            "etag": response.headers.get("ETag", ""),
        }

    def delete_object(self, bucket, key):
        """Delete an object from S3."""
        response = self._request("DELETE", f"/{bucket}/{key}")
        return {"status": response.status}

    def list_objects(self, bucket, prefix="", max_keys=1000):
        """List objects in a bucket."""
        # Build query string
        query = f"?max-keys={max_keys}"
        if prefix:
            query += f"&prefix={prefix}"

        # Sign with just the bucket path (without query string)
        resource = f"/{bucket}"
        date, auth = self._sign("GET", resource)

        url = f"{self.endpoint}/{bucket}{query}"
        headers = {"Date": date, "Authorization": auth}
        req = urllib.request.Request(url, headers=headers, method="GET")

        response = urllib.request.urlopen(req, timeout=30)
        content = response.read().decode("utf-8")

        # Parse XML response
        objects = []
        try:
            root = ET.fromstring(content)
            ns = {"s3": "http://s3.amazonaws.com/doc/2006-03-01/"}

            for item in root.findall(".//s3:Contents", ns):
                obj = {
                    "key": (
                        item.find("s3:Key", ns).text
                        if item.find("s3:Key", ns) is not None
                        else ""
                    ),
                    "size": (
                        int(item.find("s3:Size", ns).text)
                        if item.find("s3:Size", ns) is not None
                        else 0
                    ),
                    "etag": (
                        item.find("s3:ETag", ns).text
                        if item.find("s3:ETag", ns) is not None
                        else ""
                    ),
                }
                objects.append(obj)
        except ET.ParseError:
            pass

        return objects


class TestRunner:
    """Test runner for LeoFS S3 API."""

    def __init__(self, client, bucket):
        self.client = client
        self.bucket = bucket
        self.passed = 0
        self.failed = 0
        self.test_objects = []

    def run_test(self, name, test_func):
        """Run a single test."""
        try:
            test_func()
            print(f"  [PASS] {name}")
            self.passed += 1
            return True
        except AssertionError as e:
            print(f"  [FAIL] {name}: {e}")
            self.failed += 1
            return False
        except Exception as e:
            print(f"  [ERROR] {name}: {e}")
            self.failed += 1
            return False

    def test_put_small_object(self):
        """Test uploading a small text object."""
        content = b"Hello LeoFS! This is a test file."
        result = self.client.put_object(
            self.bucket, "test-small.txt", content, "text/plain"
        )
        assert result["status"] == 200, f"Expected 200, got {result['status']}"
        self.test_objects.append("test-small.txt")

    def test_get_small_object(self):
        """Test downloading the small object."""
        result = self.client.get_object(self.bucket, "test-small.txt")
        expected = b"Hello LeoFS! This is a test file."
        assert result["body"] == expected, "Content mismatch"
        assert result["content_length"] == len(expected), "Size mismatch"

    def test_head_object(self):
        """Test HEAD request for object metadata."""
        result = self.client.head_object(self.bucket, "test-small.txt")
        expected_len = len(b"Hello LeoFS! This is a test file.")
        assert (
            result["content_length"] == expected_len
        ), f"Expected {expected_len}, got {result['content_length']}"

    def test_put_binary_object(self):
        """Test uploading a binary object."""
        content = os.urandom(4096)  # 4KB random data
        result = self.client.put_object(
            self.bucket, "test-binary.bin", content, "application/octet-stream"
        )
        assert result["status"] == 200
        self.test_objects.append("test-binary.bin")
        self._binary_content = content  # Save for later verification

    def test_get_binary_object(self):
        """Test downloading the binary object."""
        result = self.client.get_object(self.bucket, "test-binary.bin")
        assert result["body"] == self._binary_content, "Binary content mismatch"

    def test_put_nested_path(self):
        """Test uploading object with directory-like path."""
        content = b"Nested file content"
        result = self.client.put_object(self.bucket, "path/to/nested/file.txt", content)
        assert result["status"] == 200
        self.test_objects.append("path/to/nested/file.txt")

    def test_get_nested_path(self):
        """Test downloading object with directory-like path."""
        result = self.client.get_object(self.bucket, "path/to/nested/file.txt")
        assert result["body"] == b"Nested file content"

    def test_put_large_object(self):
        """Test uploading a larger object (1MB)."""
        content = b"x" * (1024 * 1024)  # 1MB
        result = self.client.put_object(self.bucket, "test-large.bin", content)
        assert result["status"] == 200
        self.test_objects.append("test-large.bin")

    def test_get_large_object(self):
        """Test downloading the large object."""
        result = self.client.get_object(self.bucket, "test-large.bin")
        assert (
            len(result["body"]) == 1024 * 1024
        ), f"Expected 1MB, got {len(result['body'])}"

    def test_list_objects(self):
        """Test listing objects in bucket."""
        objects = self.client.list_objects(self.bucket)
        keys = [obj["key"] for obj in objects]
        # LeoFS returns keys with leading slash, normalize for comparison
        normalized_keys = [k.lstrip("/") for k in keys]
        assert (
            "test-small.txt" in normalized_keys
        ), f"test-small.txt not found in {keys}"

    def test_delete_object(self):
        """Test deleting an object."""
        result = self.client.delete_object(self.bucket, "test-small.txt")
        assert result["status"] in [
            200,
            204,
        ], f"Expected 200/204, got {result['status']}"
        if "test-small.txt" in self.test_objects:
            self.test_objects.remove("test-small.txt")

    def test_verify_delete(self):
        """Verify the object was deleted."""
        try:
            self.client.head_object(self.bucket, "test-small.txt")
            raise AssertionError("Object should have been deleted")
        except urllib.error.HTTPError as e:
            assert e.code == 404, f"Expected 404, got {e.code}"

    def cleanup(self):
        """Clean up test objects."""
        print("\n[Cleanup]")
        for key in self.test_objects:
            try:
                self.client.delete_object(self.bucket, key)
                print(f"  Deleted: {key}")
            except Exception as e:
                print(f"  Failed to delete {key}: {e}")

    def run_all_tests(self):
        """Run all tests."""
        print("\n" + "=" * 60)
        print("LeoFS S3 API Test Suite")
        print("=" * 60)
        print(f"Endpoint: {self.client.endpoint}")
        print(f"Bucket: {self.bucket}")
        print("=" * 60)

        print("\n[Object Operations]")
        self.run_test("PUT small object", self.test_put_small_object)
        self.run_test("GET small object", self.test_get_small_object)
        self.run_test("HEAD object", self.test_head_object)
        self.run_test("PUT binary object", self.test_put_binary_object)
        self.run_test("GET binary object", self.test_get_binary_object)
        self.run_test("PUT nested path", self.test_put_nested_path)
        self.run_test("GET nested path", self.test_get_nested_path)
        self.run_test("PUT large object (1MB)", self.test_put_large_object)
        self.run_test("GET large object (1MB)", self.test_get_large_object)
        self.run_test("LIST objects", self.test_list_objects)
        self.run_test("DELETE object", self.test_delete_object)
        self.run_test("Verify DELETE", self.test_verify_delete)

        self.cleanup()

        print("\n" + "=" * 60)
        total = self.passed + self.failed
        print(f"Results: {self.passed}/{total} tests passed")
        if self.failed == 0:
            print("All tests passed!")
        print("=" * 60)

        return self.failed


def main():
    parser = argparse.ArgumentParser(
        description="LeoFS S3 API Simple Test",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    # Test with existing bucket
    python test_s3_simple.py --bucket test-bucket-001 \\
                             --access-key YOUR_KEY \\
                             --secret-key YOUR_SECRET

    # Use environment variables
    export LEOFS_ACCESS_KEY=your_key
    export LEOFS_SECRET_KEY=your_secret
    python test_s3_simple.py --bucket test-bucket-001
""",
    )

    parser.add_argument(
        "--endpoint",
        "-e",
        default="http://127.0.0.1:8080",
        help="S3 endpoint URL (default: http://127.0.0.1:8080)",
    )
    parser.add_argument(
        "--bucket",
        "-b",
        required=True,
        help="Bucket name to use for testing (must already exist)",
    )
    parser.add_argument(
        "--access-key",
        "-a",
        default=os.environ.get("LEOFS_ACCESS_KEY", ""),
        help="AWS access key ID",
    )
    parser.add_argument(
        "--secret-key",
        "-s",
        default=os.environ.get("LEOFS_SECRET_KEY", ""),
        help="AWS secret access key",
    )

    args = parser.parse_args()

    if not args.access_key or not args.secret_key:
        parser.error("Access key and secret key are required")

    client = LeoFSS3Client(args.endpoint, args.access_key, args.secret_key)
    runner = TestRunner(client, args.bucket)

    return runner.run_all_tests()


if __name__ == "__main__":
    sys.exit(main())
