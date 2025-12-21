"""Task Index - Core indexing logic for LeoFS documents."""

from pathlib import Path
from typing import Optional

import structlog

from .config import settings
from .models import UploadEvent, IndexResult
from .s3_client import LeoFSClient
from .lancedb_handler import LanceDBHandler
from .exceptions import PermanentError, RetryableError

logger = structlog.get_logger(__name__)


class TaskIndex:
    """
    Implements the task.index processing step.

    Flow (S3 Backend - New):
    1. Fetch original file from LeoFS (S3 API)
    2. Check idempotency (skip if already processed)
    3. Add document to LanceDB table (direct S3 write)
    4. Document is immediately searchable

    Storage format:
        s3://{bucket}/.vectors/documents.lance/

    Legacy Flow (Local - Deprecated):
    1. Fetch original file from LeoFS
    2. Create LanceDB table locally
    3. Compress to tar.gz
    4. Upload to bucket
    """

    # Supported text file extensions for initial implementation
    TEXT_EXTENSIONS = {
        ".txt",
        ".md",
        ".json",
        ".xml",
        ".html",
        ".htm",
        ".csv",
        ".log",
        ".yaml",
        ".yml",
        ".ini",
        ".cfg",
        ".py",
        ".js",
        ".ts",
        ".java",
        ".go",
        ".rs",
        ".rb",
        ".c",
        ".cpp",
        ".h",
        ".hpp",
        ".cs",
        ".php",
        ".sh",
    }

    def __init__(
        self,
        s3_client: Optional[LeoFSClient] = None,
        lancedb_handler: Optional[LanceDBHandler] = None,
    ):
        self.s3 = s3_client or LeoFSClient()
        self.lance = lancedb_handler or LanceDBHandler()

    async def process(self, event: UploadEvent) -> IndexResult:
        """
        Process an upload event and create LanceDB index.

        Args:
            event: Upload event from NATS

        Returns:
            IndexResult with processing status
        """
        bucket = event.bucket
        key = event.key
        timestamp = event.timestamp

        logger.info(
            "task_index_start",
            bucket=bucket,
            key=key,
            timestamp=timestamp,
        )

        try:
            # 1. Check if file type is supported
            if not self._is_text_file(key):
                logger.info("skipping_non_text_file", key=key)
                return IndexResult(
                    status="skipped",
                    bucket=bucket,
                    key=key,
                    reason="not_text_file",
                )

            # 2. Fetch original file from LeoFS
            content_bytes, etag = self.s3.get_object(bucket, key)

            # 3. Check idempotency - skip if already processed
            if self._already_processed(bucket, etag):
                logger.info("skipping_already_processed", etag=etag)
                return IndexResult(
                    status="skipped",
                    bucket=bucket,
                    key=key,
                    etag=etag,
                    reason="already_processed",
                )

            # 4. Decode text content
            try:
                content_text = content_bytes.decode("utf-8")
            except UnicodeDecodeError:
                logger.warning("decode_failed", key=key)
                return IndexResult(
                    status="skipped",
                    bucket=bucket,
                    key=key,
                    etag=etag,
                    reason="decode_failed",
                )

            # 5. Add document to LanceDB table (direct S3 write)
            db_uri = self.lance.add_document(
                bucket=bucket,
                etag=etag,
                key=key,
                content=content_text,
                timestamp=timestamp,
            )

            # Construct vectors key for reporting
            vectors_key = (
                f"{settings.lancedb_vectors_prefix}/{settings.lancedb_table_name}"
            )

            logger.info(
                "task_index_success",
                bucket=bucket,
                key=key,
                etag=etag,
                vectors_key=vectors_key,
                db_uri=db_uri,
            )

            return IndexResult(
                status="success",
                bucket=bucket,
                key=key,
                etag=etag,
                vectors_key=vectors_key,
            )

        except PermanentError as e:
            logger.error("task_index_permanent_error", error=str(e))
            raise

        except RetryableError as e:
            logger.warning("task_index_retryable_error", error=str(e))
            raise

        except Exception as e:
            logger.error("task_index_unexpected_error", error=str(e))
            raise RetryableError(f"Unexpected error: {e}") from e

    def _is_text_file(self, key: str) -> bool:
        """Check if the file is a supported text file."""
        suffix = Path(key).suffix.lower()
        return suffix in self.TEXT_EXTENSIONS

    def _already_processed(self, bucket: str, etag: str) -> bool:
        """
        Check if this file has already been processed (idempotency).

        Uses LanceDB's document_exists to check if ETag already exists in table.
        """
        return self.lance.document_exists(bucket, etag)


# Convenience function for direct usage
async def process_upload_event(event: UploadEvent) -> IndexResult:
    """Process an upload event using default configuration."""
    task = TaskIndex()
    return await task.process(event)
