"""Document chunking using LlamaIndex."""

import structlog
from dataclasses import dataclass
from typing import Optional

from ..config import settings

logger = structlog.get_logger()

# Lazy load LlamaIndex to avoid import errors
_splitter = None


def _get_splitter(chunk_size: int, chunk_overlap: int):
    """Get or create SentenceSplitter instance."""
    global _splitter
    if _splitter is None or _splitter.chunk_size != chunk_size:
        try:
            from llama_index.core.node_parser import SentenceSplitter

            _splitter = SentenceSplitter(
                chunk_size=chunk_size,
                chunk_overlap=chunk_overlap,
                paragraph_separator="\n\n",
                secondary_chunking_regex="[^,.;。？！]+[,.;。？！]?",
            )
            logger.info(
                "sentence_splitter_initialized",
                chunk_size=chunk_size,
                chunk_overlap=chunk_overlap,
            )
        except ImportError:
            raise ImportError(
                "llama-index-core not installed. Run: pip install llama-index-core"
            )
    return _splitter


@dataclass
class Chunk:
    """A chunk of text from a document.

    Attributes:
        chunk_id: Sequential index of this chunk (0-based)
        content: The text content of this chunk
        start_char: Starting character position in original text
        end_char: Ending character position in original text
        metadata: Optional additional metadata
    """

    chunk_id: int
    content: str
    start_char: int
    end_char: int
    metadata: Optional[dict] = None

    def __len__(self) -> int:
        return len(self.content)


class ChunkingService:
    """Document chunking service using LlamaIndex SentenceSplitter.

    Splits documents into overlapping chunks while preserving sentence
    boundaries where possible.

    Attributes:
        chunk_size: Target size for each chunk in characters
        chunk_overlap: Number of characters to overlap between chunks
    """

    def __init__(
        self,
        chunk_size: Optional[int] = None,
        chunk_overlap: Optional[int] = None,
    ):
        """Initialize ChunkingService.

        Args:
            chunk_size: Target chunk size. Defaults to settings.chunk_size.
            chunk_overlap: Overlap between chunks. Defaults to settings.chunk_overlap.
        """
        self.chunk_size = chunk_size if chunk_size is not None else settings.chunk_size
        self.chunk_overlap = (
            chunk_overlap if chunk_overlap is not None else settings.chunk_overlap
        )

        logger.debug(
            "chunking_service_initialized",
            chunk_size=self.chunk_size,
            chunk_overlap=self.chunk_overlap,
        )

    def chunk(self, text: str, metadata: Optional[dict] = None) -> list[Chunk]:
        """Split text into chunks.

        Args:
            text: The text to split
            metadata: Optional metadata to attach to each chunk

        Returns:
            List of Chunk objects
        """
        if not text or not text.strip():
            logger.warning("chunking_empty_text")
            return []

        splitter = _get_splitter(self.chunk_size, self.chunk_overlap)

        # Use LlamaIndex's split_text method
        chunk_texts = splitter.split_text(text)

        chunks = []
        current_pos = 0

        for idx, chunk_text in enumerate(chunk_texts):
            # Find the position of this chunk in the original text
            # This is approximate due to overlapping
            start_char = text.find(chunk_text, current_pos)
            if start_char == -1:
                # Fallback if exact match not found
                start_char = current_pos

            end_char = start_char + len(chunk_text)

            chunk = Chunk(
                chunk_id=idx,
                content=chunk_text,
                start_char=start_char,
                end_char=end_char,
                metadata=metadata.copy() if metadata else None,
            )
            chunks.append(chunk)

            # Move position forward (accounting for overlap)
            current_pos = max(current_pos, end_char - self.chunk_overlap)

        logger.debug(
            "text_chunked",
            original_length=len(text),
            num_chunks=len(chunks),
            avg_chunk_size=sum(len(c) for c in chunks) // len(chunks) if chunks else 0,
        )

        return chunks

    def chunk_with_ids(
        self,
        text: str,
        base_id: str,
        metadata: Optional[dict] = None,
    ) -> list[Chunk]:
        """Split text into chunks with composite IDs.

        Creates chunk IDs in the format: {base_id}_{chunk_id}

        Args:
            text: The text to split
            base_id: Base identifier (e.g., etag) for composite ID
            metadata: Optional metadata to attach to each chunk

        Returns:
            List of Chunk objects with metadata containing 'composite_id'
        """
        chunks = self.chunk(text, metadata)

        for chunk in chunks:
            composite_id = f"{base_id}_{chunk.chunk_id}"
            if chunk.metadata is None:
                chunk.metadata = {}
            chunk.metadata["composite_id"] = composite_id

        return chunks


class SimpleChunker:
    """Simple fixed-size chunker without LlamaIndex dependency.

    Use this as a fallback or for simple use cases where LlamaIndex
    is not available.
    """

    def __init__(self, chunk_size: int = 512, chunk_overlap: int = 50):
        self.chunk_size = chunk_size
        self.chunk_overlap = chunk_overlap

    def chunk(self, text: str) -> list[Chunk]:
        """Split text into fixed-size chunks with overlap.

        Args:
            text: The text to split

        Returns:
            List of Chunk objects
        """
        if not text:
            return []

        chunks = []
        start = 0
        chunk_id = 0

        while start < len(text):
            end = min(start + self.chunk_size, len(text))

            # Try to break at a sentence boundary
            if end < len(text):
                # Look for sentence-ending punctuation
                for sep in ["。", ".", "!", "?", "\n"]:
                    last_sep = text.rfind(sep, start, end)
                    if last_sep > start + self.chunk_size // 2:
                        end = last_sep + 1
                        break

            chunk_text = text[start:end]
            chunks.append(
                Chunk(
                    chunk_id=chunk_id,
                    content=chunk_text,
                    start_char=start,
                    end_char=end,
                )
            )

            chunk_id += 1
            start = end - self.chunk_overlap

            # Prevent infinite loop
            if start >= len(text) - self.chunk_overlap:
                break

        return chunks
