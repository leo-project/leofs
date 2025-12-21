"""LanceDB Handler for creating and managing document tables with S3 backend."""

import os
import shutil
from pathlib import Path
from typing import Optional, List, Dict, Any

import lancedb
import pyarrow as pa
import structlog

from .config import settings
from .exceptions import LanceDBError

logger = structlog.get_logger(__name__)


class LanceDBHandler:
    """
    Handler for LanceDB operations with S3 backend support.

    Storage Strategy:
    - 1 bucket = 1 LanceDB table (avoids fragmentation)
    - Direct write to S3 (LeoFS) without tar.gz compression
    - Uses table.add() for appending new documents

    Schema:
    - id: string (ETag - primary key for idempotency)
    - bucket: string
    - key: string
    - content: string (text content)
    - created_at: int64 (Unix timestamp)

    Future extensions will add:
    - chunk_id: int (for chunked documents)
    - vector: list[float] (embeddings)
    - metadata: struct (additional info)
    """

    # Table schema for initial implementation
    SCHEMA = pa.schema(
        [
            pa.field("id", pa.string()),
            pa.field("bucket", pa.string()),
            pa.field("key", pa.string()),
            pa.field("content", pa.string()),
            pa.field("created_at", pa.int64()),
        ]
    )

    def __init__(
        self,
        s3_enabled: bool = settings.lancedb_s3_enabled,
        table_name: str = settings.lancedb_table_name,
        vectors_prefix: str = settings.lancedb_vectors_prefix,
        temp_dir: str = settings.temp_dir,
        endpoint_url: str = settings.leofs_endpoint,
        access_key: str = settings.leofs_access_key,
        secret_key: str = settings.leofs_secret_key,
    ):
        self.s3_enabled = s3_enabled
        self.table_name = table_name
        self.vectors_prefix = vectors_prefix
        self.temp_dir = Path(temp_dir)
        self.endpoint_url = endpoint_url

        # Build storage options for LanceDB S3 connection
        # LeoFS allows internal network requests without authentication
        self.storage_options = None
        if s3_enabled:
            self.storage_options = {
                "aws_endpoint": endpoint_url,
                "aws_access_key_id": access_key or "_internal_",
                "aws_secret_access_key": secret_key or "_internal_",
                "aws_region": "us-east-1",
                "allow_http": "true",
                "aws_virtual_hosted_style_request": "false",
                "aws_s3_allow_unsafe_rename": "true",
            }
            logger.debug(
                "s3_storage_options_configured",
                endpoint=endpoint_url,
            )

        # Ensure temp directory exists for local fallback
        self.temp_dir.mkdir(parents=True, exist_ok=True)

        logger.info(
            "lancedb_handler_initialized",
            s3_enabled=s3_enabled,
            table_name=table_name,
            vectors_prefix=vectors_prefix,
        )

    def _get_db_uri(self, bucket: str) -> str:
        """
        Get the database URI for a bucket.

        Args:
            bucket: Bucket name

        Returns:
            S3 URI (e.g., s3://bucket/.vectors/) or local path
        """
        if self.s3_enabled:
            return f"s3://{bucket}/{self.vectors_prefix}/"
        else:
            return str(self.temp_dir / bucket)

    def add_document(
        self,
        bucket: str,
        etag: str,
        key: str,
        content: str,
        timestamp: int,
    ) -> str:
        """
        Add a document to the bucket's LanceDB table.

        This method implements the 1-bucket-1-table strategy:
        - If table doesn't exist, create it
        - If table exists, append the document

        Args:
            bucket: Source bucket name
            etag: Object ETag (used as primary key)
            key: Source object key
            content: Text content of the document
            timestamp: Unix timestamp

        Returns:
            The table URI where document was stored

        Raises:
            LanceDBError: If operation fails
        """
        db_uri = self._get_db_uri(bucket)

        try:
            logger.debug(
                "adding_document",
                bucket=bucket,
                etag=etag,
                key=key,
                db_uri=db_uri,
            )

            # Connect to LanceDB (S3 or local)
            db = lancedb.connect(db_uri, storage_options=self.storage_options)

            # Prepare data
            data = [
                {
                    "id": etag,
                    "bucket": bucket,
                    "key": key,
                    "content": content,
                    "created_at": timestamp,
                }
            ]

            # Check if table exists
            if self.table_name in db.table_names():
                # Append to existing table
                table = db.open_table(self.table_name)
                table.add(data)
                logger.info(
                    "document_appended",
                    bucket=bucket,
                    etag=etag,
                    key=key,
                    table=self.table_name,
                )
            else:
                # Create new table
                table = db.create_table(
                    self.table_name,
                    data=data,
                    schema=self.SCHEMA,
                )
                logger.info(
                    "table_created_with_document",
                    bucket=bucket,
                    etag=etag,
                    key=key,
                    table=self.table_name,
                )

            return db_uri

        except Exception as e:
            logger.error(
                "add_document_failed",
                bucket=bucket,
                etag=etag,
                error=str(e),
            )
            raise LanceDBError(f"Failed to add document: {e}") from e

    def add_documents_batch(
        self,
        bucket: str,
        documents: List[Dict[str, Any]],
    ) -> str:
        """
        Add multiple documents to the bucket's LanceDB table.

        Args:
            bucket: Source bucket name
            documents: List of document dicts with keys:
                       id, bucket, key, content, created_at

        Returns:
            The table URI where documents were stored

        Raises:
            LanceDBError: If operation fails
        """
        if not documents:
            raise LanceDBError("No documents to add")

        db_uri = self._get_db_uri(bucket)

        try:
            logger.debug(
                "adding_documents_batch",
                bucket=bucket,
                count=len(documents),
                db_uri=db_uri,
            )

            db = lancedb.connect(db_uri, storage_options=self.storage_options)

            if self.table_name in db.table_names():
                table = db.open_table(self.table_name)
                table.add(documents)
            else:
                table = db.create_table(
                    self.table_name,
                    data=documents,
                    schema=self.SCHEMA,
                )

            logger.info(
                "documents_batch_added",
                bucket=bucket,
                count=len(documents),
                table=self.table_name,
            )

            return db_uri

        except Exception as e:
            logger.error(
                "add_documents_batch_failed",
                bucket=bucket,
                count=len(documents),
                error=str(e),
            )
            raise LanceDBError(f"Failed to add documents batch: {e}") from e

    def document_exists(self, bucket: str, etag: str) -> bool:
        """
        Check if a document with the given ETag already exists.

        Args:
            bucket: Bucket name
            etag: Document ETag to check

        Returns:
            True if document exists, False otherwise
        """
        db_uri = self._get_db_uri(bucket)

        try:
            db = lancedb.connect(db_uri, storage_options=self.storage_options)

            if self.table_name not in db.table_names():
                return False

            table = db.open_table(self.table_name)
            # Use LanceDB's filter to check for existing document
            result = table.search().where(f"id = '{etag}'").limit(1).to_list()
            return len(result) > 0

        except Exception as e:
            logger.warning(
                "document_exists_check_failed",
                bucket=bucket,
                etag=etag,
                error=str(e),
            )
            return False

    def compact_table(self, bucket: str) -> None:
        """
        Compact the table to optimize storage and query performance.

        This should be called periodically (e.g., via cron or background task)
        to merge fragmented files.

        Args:
            bucket: Bucket name
        """
        db_uri = self._get_db_uri(bucket)

        try:
            db = lancedb.connect(db_uri, storage_options=self.storage_options)

            if self.table_name not in db.table_names():
                logger.debug("compact_skipped_no_table", bucket=bucket)
                return

            table = db.open_table(self.table_name)
            table.compact_files()

            logger.info(
                "table_compacted",
                bucket=bucket,
                table=self.table_name,
            )

        except Exception as e:
            logger.error(
                "compact_failed",
                bucket=bucket,
                error=str(e),
            )

    def get_table_stats(self, bucket: str) -> Optional[Dict[str, Any]]:
        """
        Get statistics for the bucket's table.

        Args:
            bucket: Bucket name

        Returns:
            Dict with table stats or None if table doesn't exist
        """
        db_uri = self._get_db_uri(bucket)

        try:
            db = lancedb.connect(db_uri, storage_options=self.storage_options)

            if self.table_name not in db.table_names():
                return None

            table = db.open_table(self.table_name)
            return {
                "row_count": table.count_rows(),
                "uri": db_uri,
                "table_name": self.table_name,
            }

        except Exception as e:
            logger.error(
                "get_table_stats_failed",
                bucket=bucket,
                error=str(e),
            )
            return None

    def read_documents(
        self,
        bucket: str,
        limit: int = 100,
    ) -> Optional[List[Dict[str, Any]]]:
        """
        Read documents from the bucket's table.

        Args:
            bucket: Bucket name
            limit: Maximum number of documents to return

        Returns:
            List of document dicts or None if table doesn't exist
        """
        db_uri = self._get_db_uri(bucket)

        try:
            db = lancedb.connect(db_uri, storage_options=self.storage_options)

            if self.table_name not in db.table_names():
                return None

            table = db.open_table(self.table_name)
            return table.to_pandas().head(limit).to_dict(orient="records")

        except Exception as e:
            logger.error(
                "read_documents_failed",
                bucket=bucket,
                error=str(e),
            )
            return None

    # --- Legacy methods for backward compatibility ---

    def create_table(
        self,
        etag: str,
        bucket: str,
        key: str,
        content: str,
        timestamp: int,
    ) -> Path:
        """
        Legacy method: Create a LanceDB table for a document.

        This method is kept for backward compatibility but now uses
        the S3 backend when enabled.

        Returns:
            Path object (for S3, returns a pseudo-path)
        """
        db_uri = self.add_document(
            bucket=bucket,
            etag=etag,
            key=key,
            content=content,
            timestamp=timestamp,
        )
        # Return pseudo-path for compatibility
        return Path(db_uri) / self.table_name

    def cleanup(self, etag: str) -> None:
        """
        Legacy method: Cleanup is no-op for S3 backend.

        Documents are stored directly in S3, no local cleanup needed.
        """
        if not self.s3_enabled:
            db_path = self.temp_dir / f"db_{etag}"
            if db_path.exists():
                shutil.rmtree(db_path)
                logger.debug("local_cleanup", etag=etag, path=str(db_path))

    def cleanup_all(self) -> None:
        """Clean up all temporary local LanceDB directories."""
        if not self.s3_enabled and self.temp_dir.exists():
            for item in self.temp_dir.iterdir():
                if item.is_dir() and item.name.startswith("db_"):
                    shutil.rmtree(item)
            logger.info("local_cleanup_all", temp_dir=str(self.temp_dir))
