"""Task Index - Core indexing logic for LeoFS documents."""

import tarfile
import tempfile
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

    Flow:
    1. Fetch original file from LeoFS (S3 API)
    2. Check idempotency (skip if already processed)
    3. Create LanceDB table
    4. Compress .lance directory to tar.gz
    5. Upload to same bucket's .vectors/ directory
    6. Cleanup temporary files

    Storage format:
        {bucket}/.vectors/{etag}.lance.tar.gz
    """

    # Directory name for storing LanceDB indexes within the bucket
    VECTORS_DIR = ".vectors"

    # Supported text file extensions for initial implementation
    TEXT_EXTENSIONS = {
        ".txt", ".md", ".json", ".xml", ".html", ".htm",
        ".csv", ".log", ".yaml", ".yml", ".ini", ".cfg",
        ".py", ".js", ".ts", ".java", ".go", ".rs", ".rb",
        ".c", ".cpp", ".h", ".hpp", ".cs", ".php", ".sh",
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

            # 5. Create LanceDB table
            lance_dir = self.lance.create_table(
                etag=etag,
                bucket=bucket,
                key=key,
                content=content_text,
                timestamp=timestamp,
            )

            # 6. Compress to tar.gz
            archive_path = self._create_archive(lance_dir, etag)

            # 7. Upload to same bucket's .vectors/ directory
            vectors_key = f"{self.VECTORS_DIR}/{etag}.lance.tar.gz"
            self._upload_archive(archive_path, bucket, vectors_key)

            # 8. Cleanup
            self.lance.cleanup(etag)
            archive_path.unlink(missing_ok=True)

            logger.info(
                "task_index_success",
                bucket=bucket,
                key=key,
                etag=etag,
                vectors_key=vectors_key,
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

        Looks for existing .lance.tar.gz in the same bucket's .vectors/ directory.
        """
        vectors_key = f"{self.VECTORS_DIR}/{etag}.lance.tar.gz"
        return self.s3.object_exists(bucket, vectors_key)

    def _create_archive(self, lance_dir: Path, etag: str) -> Path:
        """
        Compress the .lance directory to tar.gz.

        Args:
            lance_dir: Path to .lance directory
            etag: Object ETag for naming

        Returns:
            Path to created archive
        """
        archive_path = lance_dir.parent / f"{etag}.lance.tar.gz"

        logger.debug("creating_archive", source=str(lance_dir), dest=str(archive_path))

        with tarfile.open(archive_path, "w:gz") as tar:
            # Add the .lance directory with its base name
            tar.add(lance_dir, arcname=lance_dir.name)

        logger.info(
            "archive_created",
            path=str(archive_path),
            size=archive_path.stat().st_size,
        )

        return archive_path

    def _upload_archive(self, archive_path: Path, bucket: str, vectors_key: str) -> None:
        """
        Upload the archive to the same bucket's .vectors/ directory.

        Args:
            archive_path: Local path to archive
            bucket: Target bucket (same as source bucket)
            vectors_key: Destination key (e.g., .vectors/{etag}.lance.tar.gz)
        """
        self.s3.upload_file(
            str(archive_path),
            bucket,
            vectors_key,
            content_type="application/gzip",
        )

        logger.info(
            "archive_uploaded",
            bucket=bucket,
            key=vectors_key,
        )


# Convenience function for direct usage
async def process_upload_event(event: UploadEvent) -> IndexResult:
    """Process an upload event using default configuration."""
    task = TaskIndex()
    return await task.process(event)
