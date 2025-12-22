"""Task Index - Facade for indexing pipelines."""

from typing import Optional

import structlog

from .config import settings
from .models import UploadEvent, IndexResult
from .s3_client import LeoFSClient
from .lancedb_handler import LanceDBHandler
from .pipelines import SimplePipeline, FullPipeline

logger = structlog.get_logger(__name__)


class TaskIndex:
    """Facade for indexing pipelines.

    Provides a unified interface for processing upload events
    using either simple or full pipeline based on configuration.

    Attributes:
        simple_pipeline: Phase 1 pipeline (text only)
        full_pipeline: Phase 2 pipeline (with embeddings)
    """

    def __init__(
        self,
        s3_client: Optional[LeoFSClient] = None,
        lancedb_handler: Optional[LanceDBHandler] = None,
    ):
        """Initialize TaskIndex with shared dependencies.

        Args:
            s3_client: S3 client for fetching objects
            lancedb_handler: LanceDB handler for storage
        """
        self.s3 = s3_client or LeoFSClient()
        self.lance = lancedb_handler or LanceDBHandler()

        # Initialize pipelines with shared dependencies
        self._simple_pipeline = SimplePipeline(self.s3, self.lance)
        self._full_pipeline = FullPipeline(self.s3, self.lance)

    @property
    def simple_pipeline(self) -> SimplePipeline:
        """Get simple pipeline instance."""
        return self._simple_pipeline

    @property
    def full_pipeline(self) -> FullPipeline:
        """Get full pipeline instance."""
        return self._full_pipeline

    async def process(self, event: UploadEvent) -> IndexResult:
        """Process with simple pipeline (Phase 1).

        Args:
            event: Upload event from NATS

        Returns:
            IndexResult with processing status
        """
        return await self._simple_pipeline.process(event)

    async def process_full(self, event: UploadEvent) -> IndexResult:
        """Process with full pipeline (Phase 2).

        Args:
            event: Upload event from NATS

        Returns:
            IndexResult with processing status
        """
        return await self._full_pipeline.process(event)

    async def process_auto(self, event: UploadEvent) -> IndexResult:
        """Process with configured pipeline mode.

        Uses settings.pipeline_mode to determine which pipeline:
        - 'simple': Use process() - text only
        - 'full': Use process_full() - with embeddings

        Args:
            event: Upload event from NATS

        Returns:
            IndexResult with processing status
        """
        if settings.pipeline_mode == "full":
            return await self.process_full(event)
        else:
            return await self.process(event)


# Convenience functions for direct usage
async def process_upload_event(event: UploadEvent) -> IndexResult:
    """Process an upload event using simple pipeline."""
    task = TaskIndex()
    return await task.process(event)


async def process_upload_event_full(event: UploadEvent) -> IndexResult:
    """Process an upload event using full pipeline."""
    task = TaskIndex()
    return await task.process_full(event)


async def process_upload_event_auto(event: UploadEvent) -> IndexResult:
    """Process an upload event using configured pipeline mode."""
    task = TaskIndex()
    return await task.process_auto(event)
