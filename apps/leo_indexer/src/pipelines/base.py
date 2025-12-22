"""Base pipeline class and shared utilities."""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

import structlog

from ..models import UploadEvent, IndexResult
from ..s3_client import LeoFSClient
from ..lancedb_handler import LanceDBHandler

logger = structlog.get_logger(__name__)


@dataclass
class PipelineContext:
    """Context object passed through pipeline stages.

    Holds intermediate results and metadata as the document
    flows through the pipeline.
    """

    # Input
    event: UploadEvent
    bucket: str
    key: str
    timestamp: int

    # S3 fetch results
    content_bytes: Optional[bytes] = None
    etag: Optional[str] = None

    # Decoded text
    content_text: Optional[str] = None

    # Language detection (Phase 2)
    language: Optional[str] = None
    language_confidence: Optional[float] = None

    # Preprocessing (Phase 2)
    processed_text: Optional[str] = None

    # Chunking (Phase 2)
    chunks: list = field(default_factory=list)

    # Embeddings (Phase 2)
    embedded_chunks: list = field(default_factory=list)

    # Result
    vectors_key: Optional[str] = None
    num_chunks: Optional[int] = None

    @classmethod
    def from_event(cls, event: UploadEvent) -> "PipelineContext":
        """Create context from upload event."""
        return cls(
            event=event,
            bucket=event.bucket,
            key=event.key,
            timestamp=event.timestamp,
        )


class BasePipeline(ABC):
    """Abstract base class for indexing pipelines.

    Provides common functionality and defines the interface
    that all pipelines must implement.
    """

    # Supported text file extensions
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
        """Initialize pipeline with dependencies.

        Args:
            s3_client: S3 client for fetching objects
            lancedb_handler: LanceDB handler for storage
        """
        self.s3 = s3_client or LeoFSClient()
        self.lance = lancedb_handler or LanceDBHandler()

    @abstractmethod
    async def process(self, event: UploadEvent) -> IndexResult:
        """Process an upload event.

        Args:
            event: Upload event from NATS

        Returns:
            IndexResult with processing status
        """
        pass

    @property
    @abstractmethod
    def name(self) -> str:
        """Pipeline name for logging."""
        pass

    def is_text_file(self, key: str) -> bool:
        """Check if the file is a supported text file.

        Args:
            key: Object key (path)

        Returns:
            True if file extension is in TEXT_EXTENSIONS
        """
        suffix = Path(key).suffix.lower()
        return suffix in self.TEXT_EXTENSIONS

    def fetch_object(self, ctx: PipelineContext) -> bool:
        """Fetch object from S3 and update context.

        Args:
            ctx: Pipeline context

        Returns:
            True if successful, False otherwise
        """
        content_bytes, etag = self.s3.get_object(ctx.bucket, ctx.key)
        ctx.content_bytes = content_bytes
        ctx.etag = etag
        return True

    def decode_text(self, ctx: PipelineContext) -> bool:
        """Decode bytes to text and update context.

        Args:
            ctx: Pipeline context

        Returns:
            True if successful, False if decode failed
        """
        try:
            ctx.content_text = ctx.content_bytes.decode("utf-8")
            return True
        except UnicodeDecodeError:
            logger.warning("decode_failed", key=ctx.key)
            return False

    def skip_result(
        self,
        ctx: PipelineContext,
        reason: str,
    ) -> IndexResult:
        """Create a skipped result.

        Args:
            ctx: Pipeline context
            reason: Reason for skipping

        Returns:
            IndexResult with status="skipped"
        """
        return IndexResult(
            status="skipped",
            bucket=ctx.bucket,
            key=ctx.key,
            etag=ctx.etag,
            language=ctx.language,
            reason=reason,
        )

    def success_result(
        self,
        ctx: PipelineContext,
    ) -> IndexResult:
        """Create a success result.

        Args:
            ctx: Pipeline context

        Returns:
            IndexResult with status="success"
        """
        return IndexResult(
            status="success",
            bucket=ctx.bucket,
            key=ctx.key,
            etag=ctx.etag,
            vectors_key=ctx.vectors_key,
            language=ctx.language,
            num_chunks=ctx.num_chunks,
        )
