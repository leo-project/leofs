"""LanceDB Handler for creating and managing document tables."""

import shutil
from pathlib import Path
from typing import Optional

import lancedb
import pyarrow as pa
import structlog

from .config import settings
from .exceptions import LanceDBError

logger = structlog.get_logger(__name__)


class LanceDBHandler:
    """
    Handler for LanceDB operations.

    Creates document tables with the following schema:
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
    SCHEMA = pa.schema([
        pa.field("id", pa.string()),
        pa.field("bucket", pa.string()),
        pa.field("key", pa.string()),
        pa.field("content", pa.string()),
        pa.field("created_at", pa.int64()),
    ])

    TABLE_NAME = "documents"

    def __init__(self, temp_dir: str = settings.temp_dir):
        self.temp_dir = Path(temp_dir)
        self.temp_dir.mkdir(parents=True, exist_ok=True)
        logger.info("lancedb_handler_initialized", temp_dir=str(self.temp_dir))

    def create_table(
        self,
        etag: str,
        bucket: str,
        key: str,
        content: str,
        timestamp: int,
    ) -> Path:
        """
        Create a LanceDB table for a document.

        Args:
            etag: Object ETag (used as primary key)
            bucket: Source bucket name
            key: Source object key
            content: Text content of the document
            timestamp: Unix timestamp

        Returns:
            Path to the created .lance directory

        Raises:
            LanceDBError: If table creation fails
        """
        db_path = self.temp_dir / f"db_{etag}"

        try:
            logger.debug(
                "creating_lancedb_table",
                etag=etag,
                bucket=bucket,
                key=key,
                db_path=str(db_path),
            )

            # Clean up existing directory if present
            if db_path.exists():
                shutil.rmtree(db_path)

            # Connect to LanceDB
            db = lancedb.connect(str(db_path))

            # Prepare data
            data = [{
                "id": etag,
                "bucket": bucket,
                "key": key,
                "content": content,
                "created_at": timestamp,
            }]

            # Create table with schema
            table = db.create_table(
                self.TABLE_NAME,
                data=data,
                schema=self.SCHEMA,
                mode="overwrite",
            )

            # Get the .lance directory path
            lance_dir = db_path / f"{self.TABLE_NAME}.lance"

            logger.info(
                "lancedb_table_created",
                etag=etag,
                lance_dir=str(lance_dir),
                row_count=table.count_rows(),
            )

            return lance_dir

        except Exception as e:
            logger.error("lancedb_create_failed", etag=etag, error=str(e))
            raise LanceDBError(f"Failed to create LanceDB table: {e}") from e

    def create_table_with_chunks(
        self,
        etag: str,
        bucket: str,
        key: str,
        chunks: list[str],
        timestamp: int,
    ) -> Path:
        """
        Create a LanceDB table for a chunked document.

        This is a placeholder for future chunking support.
        Each chunk will be stored as a separate row with chunk_id.

        Args:
            etag: Object ETag
            bucket: Source bucket name
            key: Source object key
            chunks: List of text chunks
            timestamp: Unix timestamp

        Returns:
            Path to the created .lance directory
        """
        db_path = self.temp_dir / f"db_{etag}"

        try:
            if db_path.exists():
                shutil.rmtree(db_path)

            db = lancedb.connect(str(db_path))

            # Prepare data with chunk IDs
            data = [
                {
                    "id": f"{etag}_{i}",
                    "bucket": bucket,
                    "key": key,
                    "content": chunk,
                    "created_at": timestamp,
                }
                for i, chunk in enumerate(chunks)
            ]

            table = db.create_table(
                self.TABLE_NAME,
                data=data,
                schema=self.SCHEMA,
                mode="overwrite",
            )

            lance_dir = db_path / f"{self.TABLE_NAME}.lance"

            logger.info(
                "lancedb_chunked_table_created",
                etag=etag,
                chunk_count=len(chunks),
                lance_dir=str(lance_dir),
            )

            return lance_dir

        except Exception as e:
            raise LanceDBError(f"Failed to create chunked LanceDB table: {e}") from e

    def get_db_path(self, etag: str) -> Path:
        """Get the database path for a given ETag."""
        return self.temp_dir / f"db_{etag}"

    def get_lance_dir(self, etag: str) -> Path:
        """Get the .lance directory path for a given ETag."""
        return self.get_db_path(etag) / f"{self.TABLE_NAME}.lance"

    def cleanup(self, etag: str) -> None:
        """
        Clean up temporary files for a given ETag.

        Args:
            etag: Object ETag to clean up
        """
        db_path = self.get_db_path(etag)
        if db_path.exists():
            shutil.rmtree(db_path)
            logger.debug("lancedb_cleanup", etag=etag, path=str(db_path))

    def cleanup_all(self) -> None:
        """Clean up all temporary LanceDB directories."""
        if self.temp_dir.exists():
            for item in self.temp_dir.iterdir():
                if item.is_dir() and item.name.startswith("db_"):
                    shutil.rmtree(item)
            logger.info("lancedb_cleanup_all", temp_dir=str(self.temp_dir))

    def read_table(self, etag: str) -> Optional[list[dict]]:
        """
        Read data from an existing LanceDB table.

        Args:
            etag: Object ETag

        Returns:
            List of row dictionaries or None if not found
        """
        db_path = self.get_db_path(etag)
        if not db_path.exists():
            return None

        try:
            db = lancedb.connect(str(db_path))
            table = db.open_table(self.TABLE_NAME)
            return table.to_pandas().to_dict(orient="records")
        except Exception as e:
            logger.error("lancedb_read_failed", etag=etag, error=str(e))
            return None
