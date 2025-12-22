"""Pydantic models for data validation."""

from pydantic import BaseModel, Field
from typing import Optional


class UploadEvent(BaseModel):
    """Event received from NATS JetStream when a file is uploaded to LeoFS."""

    event: str = Field(..., description="Event type (e.g., 'object_created')")
    bucket: str = Field(..., description="S3 bucket name")
    key: str = Field(..., description="Object key (path)")
    timestamp: int = Field(..., description="Unix timestamp when the event occurred")
    node_id: Optional[str] = Field(default=None, description="LeoFS node identifier")


class IndexResult(BaseModel):
    """Result of indexing operation."""

    status: str = Field(..., description="Status: 'success', 'skipped', 'error'")
    bucket: str = Field(..., description="Source bucket")
    key: str = Field(..., description="Source key")
    etag: Optional[str] = Field(
        default=None, description="Object ETag (used as primary key)"
    )
    vectors_key: Optional[str] = Field(
        default=None, description="Key where LanceDB file was stored"
    )
    reason: Optional[str] = Field(default=None, description="Reason for skip or error")
    error: Optional[str] = Field(default=None, description="Error message if failed")
    # Phase 2: Additional fields
    language: Optional[str] = Field(
        default=None, description="Detected language code (e.g., 'ja', 'en')"
    )
    num_chunks: Optional[int] = Field(
        default=None, description="Number of chunks created"
    )


class DocumentChunk(BaseModel):
    """A chunk of document with embedding for LanceDB storage.

    This model represents the schema for LanceDB table entries.
    """

    id: str = Field(..., description="Composite ID: {etag}_{chunk_id}")
    bucket: str = Field(..., description="Source bucket name")
    key: str = Field(..., description="Source object key")
    etag: str = Field(..., description="Original file ETag")
    chunk_id: int = Field(..., description="Chunk index (0-based)")
    content: str = Field(..., description="Chunk text content")
    vector: list[float] = Field(..., description="Embedding vector")
    language: str = Field(..., description="Detected language code")
    created_at: int = Field(..., description="Unix timestamp")


class LanguageDetectionResult(BaseModel):
    """Result of language detection."""

    language: str = Field(..., description="Detected language code (ISO 639-1)")
    confidence: float = Field(..., description="Detection confidence (0.0 - 1.0)")
    is_supported: bool = Field(..., description="Whether the language is supported")
    sample_chars: int = Field(
        ..., description="Number of characters used for detection"
    )
