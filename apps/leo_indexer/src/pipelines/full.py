"""Full pipeline - Phase 2 (language detection, chunking, embeddings)."""

import structlog

from ..config import settings
from ..models import UploadEvent, IndexResult
from ..exceptions import PermanentError, RetryableError
from ..text import LanguageRouter, TextPreprocessor, ChunkingService, EmbeddingService
from .base import BasePipeline, PipelineContext

logger = structlog.get_logger(__name__)

# Lazy-loaded service instances
_language_router = None
_preprocessor = None
_chunking_service = None
_embedding_services: dict[str, EmbeddingService] = {}


def _get_language_router() -> LanguageRouter:
    global _language_router
    if _language_router is None:
        _language_router = LanguageRouter()
    return _language_router


def _get_preprocessor() -> TextPreprocessor:
    global _preprocessor
    if _preprocessor is None:
        _preprocessor = TextPreprocessor()
    return _preprocessor


def _get_chunking_service() -> ChunkingService:
    global _chunking_service
    if _chunking_service is None:
        _chunking_service = ChunkingService()
    return _chunking_service


def _get_embedding_service(language: str) -> EmbeddingService:
    """Get or create language-specific embedding service.

    Args:
        language: Language code ('ja', 'en')

    Returns:
        EmbeddingService configured for the specified language
    """
    global _embedding_services
    if language not in _embedding_services:
        _embedding_services[language] = EmbeddingService(language=language)
    return _embedding_services[language]


class FullPipeline(BasePipeline):
    """Full indexing pipeline with embeddings (Phase 2).

    Flow:
    1. Check if file type is supported
    2. Fetch original file from LeoFS (S3 API)
    3. Check idempotency (skip if already processed)
    4. Decode text content
    5. Detect language (fasttext)
    6. Skip if unsupported language
    7. Preprocess text (normalization)
    8. Chunk document (LlamaIndex)
    9. Generate embeddings (sentence-transformers)
    10. Store in LanceDB with vectors

    Storage format:
        s3://{bucket}/.vectors/documents_vectors.lance/
    """

    @property
    def name(self) -> str:
        return "full"

    async def process(self, event: UploadEvent) -> IndexResult:
        """Process an upload event with full pipeline.

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
            if self.lance.etag_exists(ctx.bucket, ctx.etag):
                logger.info("skipping_already_processed", etag=ctx.etag)
                return self.skip_result(ctx, "already_processed")

            # 4. Decode text content
            if not self.decode_text(ctx):
                return self.skip_result(ctx, "decode_failed")

            # 5. Detect language
            language_router = _get_language_router()
            should_process, lang_result = language_router.should_process(
                ctx.content_text
            )

            ctx.language = lang_result.language
            ctx.language_confidence = lang_result.confidence

            # 6. Skip if unsupported language
            if not should_process:
                logger.info(
                    "skipping_unsupported_language",
                    key=ctx.key,
                    language=ctx.language,
                    confidence=ctx.language_confidence,
                )
                return self.skip_result(ctx, f"unsupported_language:{ctx.language}")

            logger.debug(
                "language_detected",
                key=ctx.key,
                language=ctx.language,
                confidence=ctx.language_confidence,
            )

            # 7. Preprocess text
            preprocessor = _get_preprocessor()
            ctx.processed_text = preprocessor.process(ctx.content_text, ctx.language)

            # 8. Chunk document
            chunking_service = _get_chunking_service()
            ctx.chunks = chunking_service.chunk(ctx.processed_text)

            if not ctx.chunks:
                logger.warning("no_chunks_created", key=ctx.key)
                return self.skip_result(ctx, "no_chunks")

            logger.debug(
                "chunks_created",
                key=ctx.key,
                num_chunks=len(ctx.chunks),
            )

            # 9. Generate embeddings (language-specific model)
            embedding_service = _get_embedding_service(ctx.language)
            ctx.embedded_chunks = embedding_service.embed_chunks(ctx.chunks, ctx.etag)

            # 10. Prepare data for LanceDB
            lance_data = []
            for ec in ctx.embedded_chunks:
                lance_data.append(
                    {
                        "id": ec.composite_id,
                        "bucket": ctx.bucket,
                        "key": ctx.key,
                        "etag": ctx.etag,
                        "chunk_id": ec.chunk_id,
                        "content": ec.content,
                        "vector": ec.vector,
                        "language": ctx.language,
                        "created_at": ctx.timestamp,
                    }
                )

            # 11. Store in LanceDB with vectors
            db_uri = self.lance.add_embedded_chunks(
                bucket=ctx.bucket,
                chunks=lance_data,
            )

            # Set result fields
            ctx.vectors_key = f"{settings.lancedb_vectors_prefix}/{settings.lancedb_table_name}_vectors"
            ctx.num_chunks = len(ctx.embedded_chunks)

            logger.info(
                "pipeline_success",
                pipeline=self.name,
                bucket=ctx.bucket,
                key=ctx.key,
                etag=ctx.etag,
                language=ctx.language,
                num_chunks=ctx.num_chunks,
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
