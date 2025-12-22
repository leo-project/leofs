"""Simple pipeline - Phase 1 (text storage only)."""

import structlog

from ..config import settings
from ..models import UploadEvent, IndexResult
from ..exceptions import PermanentError, RetryableError
from .base import BasePipeline, PipelineContext

logger = structlog.get_logger(__name__)


class SimplePipeline(BasePipeline):
    """Simple indexing pipeline (Phase 1).

    Flow:
    1. Check if file type is supported
    2. Fetch original file from LeoFS (S3 API)
    3. Check idempotency (skip if already processed)
    4. Decode text content
    5. Add document to LanceDB table (direct S3 write)

    Storage format:
        s3://{bucket}/.vectors/documents.lance/
    """

    @property
    def name(self) -> str:
        return "simple"

    async def process(self, event: UploadEvent) -> IndexResult:
        """Process an upload event with simple pipeline.

        Args:
            event: Upload event from NATS

        Returns:
            IndexResult with processing status
        """
        ctx = PipelineContext.from_event(event)

        logger.info(
            "pipeline_start",
            pipeline=self.name,
            bucket=ctx.bucket,
            key=ctx.key,
        )

        try:
            # 1. Check if file type is supported
            if not self.is_text_file(ctx.key):
                logger.info("skipping_non_text_file", key=ctx.key)
                return self.skip_result(ctx, "not_text_file")

            # 2. Fetch original file from LeoFS
            self.fetch_object(ctx)

            # 3. Check idempotency - skip if already processed
            if self.lance.document_exists(ctx.bucket, ctx.etag):
                logger.info("skipping_already_processed", etag=ctx.etag)
                return self.skip_result(ctx, "already_processed")

            # 4. Decode text content
            if not self.decode_text(ctx):
                return self.skip_result(ctx, "decode_failed")

            # 5. Add document to LanceDB table
            db_uri = self.lance.add_document(
                bucket=ctx.bucket,
                etag=ctx.etag,
                key=ctx.key,
                content=ctx.content_text,
                timestamp=ctx.timestamp,
            )

            # Set result fields
            ctx.vectors_key = (
                f"{settings.lancedb_vectors_prefix}/{settings.lancedb_table_name}"
            )

            logger.info(
                "pipeline_success",
                pipeline=self.name,
                bucket=ctx.bucket,
                key=ctx.key,
                etag=ctx.etag,
                db_uri=db_uri,
            )

            return self.success_result(ctx)

        except PermanentError as e:
            logger.error("pipeline_permanent_error", pipeline=self.name, error=str(e))
            raise

        except RetryableError as e:
            logger.warning("pipeline_retryable_error", pipeline=self.name, error=str(e))
            raise

        except Exception as e:
            logger.error("pipeline_unexpected_error", pipeline=self.name, error=str(e))
            raise RetryableError(
                f"Unexpected error in {self.name} pipeline: {e}"
            ) from e
