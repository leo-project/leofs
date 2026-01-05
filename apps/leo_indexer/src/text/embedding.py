"""Embedding generation using sentence-transformers."""

import structlog
from dataclasses import dataclass
from typing import Optional

from ..config import settings
from .chunking import Chunk

logger = structlog.get_logger()

# Lazy load models to avoid slow startup
# Cache models by name to support multiple models simultaneously
_models: dict[str, object] = {}


def _get_model(model_name: str):
    """Get or create SentenceTransformer model.

    Args:
        model_name: Hugging Face model name

    Returns:
        Loaded SentenceTransformer model
    """
    global _models

    if model_name not in _models:
        try:
            from sentence_transformers import SentenceTransformer

            logger.info("loading_embedding_model", model=model_name)
            model = SentenceTransformer(model_name)
            _models[model_name] = model

            # Log model info
            embedding_dim = model.get_sentence_embedding_dimension()
            logger.info(
                "embedding_model_loaded",
                model=model_name,
                embedding_dimension=embedding_dim,
            )
        except ImportError:
            raise ImportError(
                "sentence-transformers not installed. Run: pip install sentence-transformers"
            )

    return _models[model_name]


def _get_model_for_language(language: str) -> str:
    """Get the model name for a specific language.

    Args:
        language: Language code (e.g., 'ja', 'en')

    Returns:
        Model name to use for the language
    """
    if language == "ja":
        return settings.embedding_model_ja
    elif language == "en":
        return settings.embedding_model_en
    else:
        # Fallback to default multilingual model
        return settings.embedding_model


def _get_dimension_for_language(language: str) -> int:
    """Get the embedding dimension for a specific language.

    Args:
        language: Language code (e.g., 'ja', 'en')

    Returns:
        Embedding dimension for the language's model
    """
    if language == "ja":
        return settings.embedding_dimension_ja
    elif language == "en":
        return settings.embedding_dimension_en
    else:
        return settings.embedding_dimension


@dataclass
class EmbeddedChunk:
    """A chunk with its embedding vector.

    Attributes:
        chunk_id: Sequential index of this chunk
        content: The text content
        vector: The embedding vector
        composite_id: Composite ID ({etag}_{chunk_id})
        metadata: Additional metadata
    """

    chunk_id: int
    content: str
    vector: list[float]
    composite_id: Optional[str] = None
    metadata: Optional[dict] = None


class EmbeddingService:
    """Embedding generation service using sentence-transformers.

    Supports language-specific models for Japanese and English text.
    Uses batching for efficient processing of multiple chunks.

    Attributes:
        language: Language code for model selection ('ja', 'en', or None for default)
        model_name: Name of the sentence-transformers model
        batch_size: Number of texts to embed in one batch
        embedding_dimension: Dimension of output vectors
    """

    # Prefix for E5 models (improves retrieval quality)
    E5_QUERY_PREFIX = "query: "
    E5_PASSAGE_PREFIX = "passage: "

    def __init__(
        self,
        language: Optional[str] = None,
        model_name: Optional[str] = None,
        batch_size: Optional[int] = None,
        use_e5_prefix: bool = True,
    ):
        """Initialize EmbeddingService.

        Args:
            language: Language code ('ja', 'en') for automatic model selection.
                      If provided, overrides model_name with language-specific model.
            model_name: Sentence-transformers model name (used if language is None).
                        Defaults to settings.embedding_model.
            batch_size: Batch size for embedding generation.
                        Defaults to settings.embedding_batch_size.
            use_e5_prefix: Add E5-style prefixes for better retrieval.
                           Only applies to E5 models.
        """
        self.language = language

        # Language-specific model selection
        if language is not None:
            self.model_name = _get_model_for_language(language)
        else:
            self.model_name = (
                model_name if model_name is not None else settings.embedding_model
            )

        self.batch_size = (
            batch_size if batch_size is not None else settings.embedding_batch_size
        )
        self.use_e5_prefix = use_e5_prefix and "e5" in self.model_name.lower()

        self._embedding_dimension: Optional[int] = None

        logger.debug(
            "embedding_service_initialized",
            language=language,
            model=self.model_name,
            batch_size=self.batch_size,
            use_e5_prefix=self.use_e5_prefix,
        )

    @property
    def embedding_dimension(self) -> int:
        """Get the embedding dimension of the model."""
        if self._embedding_dimension is None:
            model = _get_model(self.model_name)
            self._embedding_dimension = model.get_sentence_embedding_dimension()
        return self._embedding_dimension

    def _prepare_text(self, text: str, is_query: bool = False) -> str:
        """Prepare text with appropriate prefix for E5 models.

        Args:
            text: Input text
            is_query: True if this is a search query, False for documents

        Returns:
            Text with appropriate prefix (if using E5 model)
        """
        if not self.use_e5_prefix:
            return text

        prefix = self.E5_QUERY_PREFIX if is_query else self.E5_PASSAGE_PREFIX
        return prefix + text

    def embed(self, texts: list[str], is_query: bool = False) -> list[list[float]]:
        """Generate embeddings for a list of texts.

        Args:
            texts: List of texts to embed
            is_query: True if these are search queries (affects E5 prefix)

        Returns:
            List of embedding vectors
        """
        if not texts:
            return []

        model = _get_model(self.model_name)

        # Prepare texts with prefixes if needed
        prepared_texts = [self._prepare_text(t, is_query) for t in texts]

        # Generate embeddings in batches
        all_embeddings = []
        for i in range(0, len(prepared_texts), self.batch_size):
            batch = prepared_texts[i : i + self.batch_size]
            embeddings = model.encode(
                batch,
                convert_to_numpy=True,
                show_progress_bar=False,
            )
            all_embeddings.extend(embeddings.tolist())

        logger.debug(
            "embeddings_generated",
            num_texts=len(texts),
            embedding_dimension=self.embedding_dimension,
        )

        return all_embeddings

    def embed_single(self, text: str, is_query: bool = False) -> list[float]:
        """Generate embedding for a single text.

        Args:
            text: Text to embed
            is_query: True if this is a search query

        Returns:
            Embedding vector
        """
        embeddings = self.embed([text], is_query)
        return embeddings[0] if embeddings else []

    def embed_chunks(
        self,
        chunks: list[Chunk],
        etag: str,
    ) -> list[EmbeddedChunk]:
        """Generate embeddings for a list of chunks.

        Args:
            chunks: List of Chunk objects
            etag: Original document ETag for composite ID

        Returns:
            List of EmbeddedChunk objects with vectors
        """
        if not chunks:
            return []

        # Extract text content
        texts = [chunk.content for chunk in chunks]

        # Generate embeddings
        vectors = self.embed(texts, is_query=False)

        # Create EmbeddedChunk objects
        embedded_chunks = []
        for chunk, vector in zip(chunks, vectors):
            composite_id = f"{etag}_{chunk.chunk_id}"
            embedded_chunk = EmbeddedChunk(
                chunk_id=chunk.chunk_id,
                content=chunk.content,
                vector=vector,
                composite_id=composite_id,
                metadata=chunk.metadata,
            )
            embedded_chunks.append(embedded_chunk)

        logger.debug(
            "chunks_embedded",
            num_chunks=len(chunks),
            etag=etag,
        )

        return embedded_chunks

    def embed_query(self, query: str) -> list[float]:
        """Generate embedding for a search query.

        Uses query prefix for E5 models to improve retrieval quality.

        Args:
            query: Search query text

        Returns:
            Query embedding vector
        """
        return self.embed_single(query, is_query=True)
