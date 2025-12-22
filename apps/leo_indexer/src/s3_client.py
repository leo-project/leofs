"""LeoFS S3 API Client using boto3."""

from typing import Tuple, Optional
from pathlib import Path

import boto3
from botocore.config import Config as BotoConfig
from botocore.exceptions import ClientError
from tenacity import (
    retry,
    stop_after_attempt,
    wait_exponential,
    retry_if_exception_type,
)
import structlog

from .config import settings
from .exceptions import S3Error, RetryableError, PermanentError

logger = structlog.get_logger(__name__)


class LeoFSClient:
    """
    S3-compatible client for LeoFS operations.

    Features:
    - Get/Put objects with ETag tracking
    - Automatic retry for transient failures
    - Path-style addressing for S3 compatibility
    """

    def __init__(
        self,
        endpoint_url: str = settings.leofs_endpoint,
        access_key: str = settings.leofs_access_key,
        secret_key: str = settings.leofs_secret_key,
    ):
        self.endpoint_url = endpoint_url

        # Check if credentials are provided
        # If not, use dummy credentials (for internal network access)
        # leo_gateway will bypass authentication for internal network requests
        use_internal_network = not access_key or not secret_key

        if use_internal_network:
            # Internal network mode: use dummy credentials
            # leo_gateway bypasses signature verification for internal network IPs
            access_key = "_internal_"
            secret_key = "_internal_"
            logger.info(
                "s3_client_initialized",
                endpoint=endpoint_url,
                mode="internal_network",
            )
        else:
            logger.info(
                "s3_client_initialized",
                endpoint=endpoint_url,
                mode="authenticated",
            )

        try:
            self._client = boto3.client(
                "s3",
                endpoint_url=endpoint_url,
                aws_access_key_id=access_key,
                aws_secret_access_key=secret_key,
                region_name="us-east-1",
                config=BotoConfig(
                    signature_version="s3v4",
                    s3={"addressing_style": "path"},
                    retries={"max_attempts": 0},
                ),
            )
        except Exception as e:
            logger.error(
                "s3_client_creation_failed",
                endpoint=endpoint_url,
                error_type=type(e).__name__,
                error=str(e),
            )
            raise

    @retry(
        stop=stop_after_attempt(3),
        wait=wait_exponential(multiplier=1, min=1, max=10),
        retry=retry_if_exception_type(RetryableError),
        reraise=True,
    )
    def get_object(self, bucket: str, key: str) -> Tuple[bytes, str]:
        """
        Get object from LeoFS.

        Args:
            bucket: Bucket name
            key: Object key

        Returns:
            Tuple of (content bytes, etag)

        Raises:
            PermanentError: Object not found or access denied
            RetryableError: Temporary network issues
        """
        logger.debug("getting_object", bucket=bucket, key=key)

        try:
            response = self._client.get_object(Bucket=bucket, Key=key)
            content = response["Body"].read()
            etag = response["ETag"].strip('"')

            logger.info(
                "object_retrieved",
                bucket=bucket,
                key=key,
                size=len(content),
                etag=etag,
            )
            return content, etag

        except ClientError as e:
            error_code = e.response.get("Error", {}).get("Code", "Unknown")
            error_msg = e.response.get("Error", {}).get("Message", str(e))

            if error_code in ("NoSuchKey", "NoSuchBucket", "404"):
                raise PermanentError(f"Object not found: {bucket}/{key}") from e
            elif error_code in ("AccessDenied", "403"):
                raise PermanentError(f"Access denied: {bucket}/{key}") from e
            elif error_code in ("ServiceUnavailable", "503", "InternalError", "500"):
                raise RetryableError(f"Service unavailable: {error_msg}") from e
            else:
                raise S3Error(f"S3 error ({error_code}): {error_msg}") from e

        except Exception as e:
            raise RetryableError(f"Network error: {e}") from e

    @retry(
        stop=stop_after_attempt(3),
        wait=wait_exponential(multiplier=1, min=1, max=10),
        retry=retry_if_exception_type(RetryableError),
        reraise=True,
    )
    def put_object(
        self,
        bucket: str,
        key: str,
        body: bytes,
        content_type: Optional[str] = None,
    ) -> str:
        """
        Put object to LeoFS.

        Args:
            bucket: Bucket name
            key: Object key
            body: Content bytes
            content_type: Optional content type

        Returns:
            ETag of the uploaded object

        Raises:
            PermanentError: Bucket not found or access denied
            RetryableError: Temporary network issues
        """
        logger.debug("putting_object", bucket=bucket, key=key, size=len(body))

        try:
            extra_args = {}
            if content_type:
                extra_args["ContentType"] = content_type

            response = self._client.put_object(
                Bucket=bucket,
                Key=key,
                Body=body,
                **extra_args,
            )
            etag = response.get("ETag", "").strip('"')

            logger.info(
                "object_uploaded",
                bucket=bucket,
                key=key,
                size=len(body),
                etag=etag,
            )
            return etag

        except ClientError as e:
            error_code = e.response.get("Error", {}).get("Code", "Unknown")
            error_msg = e.response.get("Error", {}).get("Message", str(e))

            if error_code in ("NoSuchBucket", "404"):
                raise PermanentError(f"Bucket not found: {bucket}") from e
            elif error_code in ("AccessDenied", "403"):
                raise PermanentError(f"Access denied: {bucket}/{key}") from e
            elif error_code in ("ServiceUnavailable", "503", "InternalError", "500"):
                raise RetryableError(f"Service unavailable: {error_msg}") from e
            else:
                raise S3Error(f"S3 error ({error_code}): {error_msg}") from e

        except Exception as e:
            raise RetryableError(f"Network error: {e}") from e

    @retry(
        stop=stop_after_attempt(3),
        wait=wait_exponential(multiplier=1, min=1, max=10),
        retry=retry_if_exception_type(RetryableError),
        reraise=True,
    )
    def upload_file(
        self,
        local_path: str,
        bucket: str,
        key: str,
        content_type: Optional[str] = None,
    ) -> None:
        """
        Upload a local file to LeoFS.

        Args:
            local_path: Path to local file
            bucket: Bucket name
            key: Object key
            content_type: Optional content type
        """
        logger.debug("uploading_file", path=local_path, bucket=bucket, key=key)

        try:
            extra_args = {}
            if content_type:
                extra_args["ContentType"] = content_type

            self._client.upload_file(
                local_path,
                bucket,
                key,
                ExtraArgs=extra_args if extra_args else None,
            )

            logger.info("file_uploaded", path=local_path, bucket=bucket, key=key)

        except ClientError as e:
            error_code = e.response.get("Error", {}).get("Code", "Unknown")
            error_msg = e.response.get("Error", {}).get("Message", str(e))

            if error_code in ("NoSuchBucket", "404"):
                raise PermanentError(f"Bucket not found: {bucket}") from e
            elif error_code in ("AccessDenied", "403"):
                raise PermanentError(f"Access denied: {bucket}/{key}") from e
            else:
                raise RetryableError(f"Upload failed: {error_msg}") from e

        except FileNotFoundError as e:
            raise PermanentError(f"Local file not found: {local_path}") from e

        except Exception as e:
            raise RetryableError(f"Network error: {e}") from e

    def head_object(self, bucket: str, key: str) -> Optional[dict]:
        """
        Check if object exists and get metadata.

        Args:
            bucket: Bucket name
            key: Object key

        Returns:
            Object metadata dict or None if not found
        """
        try:
            response = self._client.head_object(Bucket=bucket, Key=key)
            return {
                "etag": response.get("ETag", "").strip('"'),
                "content_length": response.get("ContentLength"),
                "content_type": response.get("ContentType"),
                "last_modified": response.get("LastModified"),
            }
        except ClientError as e:
            if e.response.get("Error", {}).get("Code") in ("404", "NoSuchKey"):
                return None
            raise

    def object_exists(self, bucket: str, key: str) -> bool:
        """Check if an object exists."""
        return self.head_object(bucket, key) is not None

    def list_objects(
        self,
        bucket: str,
        prefix: str = "",
        max_keys: int = 1000,
    ) -> list:
        """
        List objects in a bucket with optional prefix.

        Args:
            bucket: Bucket name
            prefix: Key prefix filter
            max_keys: Maximum number of keys to return

        Returns:
            List of object keys
        """
        try:
            response = self._client.list_objects_v2(
                Bucket=bucket,
                Prefix=prefix,
                MaxKeys=max_keys,
            )
            contents = response.get("Contents", [])
            return [obj["Key"] for obj in contents]
        except ClientError as e:
            logger.error("list_objects_failed", bucket=bucket, error=str(e))
            return []
