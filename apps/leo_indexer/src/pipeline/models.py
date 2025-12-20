"""Pydantic models for data validation."""

from pydantic import BaseModel, Field
from typing import Optional


class UploadEvent(BaseModel):
    """Event received from NATS JetStream when a file is uploaded to LeoFS."""

    event: str = Field(
        ...,
        description="Event type (e.g., 'object_created')"
    )
    bucket: str = Field(
        ...,
        description="S3 bucket name"
    )
    key: str = Field(
        ...,
        description="Object key (path)"
    )
    timestamp: int = Field(
        ...,
        description="Unix timestamp when the event occurred"
    )
    node_id: Optional[str] = Field(
        default=None,
        description="LeoFS node identifier"
    )


class IndexResult(BaseModel):
    """Result of indexing operation."""

    status: str = Field(
        ...,
        description="Status: 'success', 'skipped', 'error'"
    )
    bucket: str = Field(
        ...,
        description="Source bucket"
    )
    key: str = Field(
        ...,
        description="Source key"
    )
    etag: Optional[str] = Field(
        default=None,
        description="Object ETag (used as primary key)"
    )
    vectors_key: Optional[str] = Field(
        default=None,
        description="Key where LanceDB file was stored"
    )
    reason: Optional[str] = Field(
        default=None,
        description="Reason for skip or error"
    )
    error: Optional[str] = Field(
        default=None,
        description="Error message if failed"
    )
