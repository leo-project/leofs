"""Text processing module for leo_indexer.

This module provides text processing components:
- Language detection (fasttext)
- Text preprocessing (normalization)
- Document chunking (LlamaIndex)
- Embedding generation (sentence-transformers)
"""

from .language import LanguageRouter, LanguageResult
from .preprocessor import TextPreprocessor, HTMLCleaner
from .chunking import ChunkingService, Chunk, SimpleChunker
from .embedding import (
    EmbeddingService,
    EmbeddedChunk,
    _get_model_for_language,
    _get_dimension_for_language,
)

__all__ = [
    # Language detection
    "LanguageRouter",
    "LanguageResult",
    # Preprocessing
    "TextPreprocessor",
    "HTMLCleaner",
    # Chunking
    "ChunkingService",
    "Chunk",
    "SimpleChunker",
    # Embedding
    "EmbeddingService",
    "EmbeddedChunk",
    # Helper functions
    "_get_model_for_language",
    "_get_dimension_for_language",
]
