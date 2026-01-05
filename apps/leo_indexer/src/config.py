"""Configuration management using pydantic-settings."""

from pydantic_settings import BaseSettings
from pydantic import Field


class Settings(BaseSettings):
    """Application settings loaded from environment variables."""

    # NATS Configuration
    nats_url: str = Field(
        default="nats://localhost:4222", description="NATS server URL"
    )
    nats_stream: str = Field(
        default="LEOFS_EVENTS", description="JetStream stream name"
    )
    nats_subject: str = Field(
        default="leofs.events.upload", description="Subject to subscribe to"
    )
    nats_consumer: str = Field(
        default="leo-indexer", description="Durable consumer name"
    )
    nats_max_deliver: int = Field(
        default=3, description="Max delivery attempts before DLQ"
    )
    nats_ack_wait: int = Field(default=30, description="ACK wait time in seconds")

    # LeoFS S3 Configuration
    leofs_endpoint: str = Field(
        default="http://localhost:8080", description="LeoFS S3 API endpoint"
    )
    leofs_access_key: str = Field(default="", description="LeoFS access key")
    leofs_secret_key: str = Field(default="", description="LeoFS secret key")

    # Storage Configuration
    temp_dir: str = Field(
        default="/tmp/lance",
        description="Temporary directory for LanceDB (used for local fallback)",
    )
    vectors_bucket: str = Field(
        default="",
        description="Bucket for vector storage (empty = same as source bucket)",
    )

    # LanceDB S3 Backend Configuration
    lancedb_s3_enabled: bool = Field(
        default=True,
        description="Enable S3 backend for LanceDB (direct write to LeoFS)",
    )
    lancedb_table_name: str = Field(
        default="documents", description="LanceDB table name (1 table per bucket)"
    )
    lancedb_vectors_prefix: str = Field(
        default=".vectors", description="Prefix for LanceDB storage in bucket"
    )

    # DLQ Configuration
    dlq_subject: str = Field(
        default="leofs.events.dlq", description="Dead Letter Queue subject"
    )

    # Logging
    log_level: str = Field(
        default="INFO", description="Log level (DEBUG, INFO, WARNING, ERROR)"
    )

    # Phase 2: Language Detection Configuration
    fasttext_model_path: str = Field(
        default="models/lid.176.ftz",
        description="Path to fasttext language detection model",
    )
    language_detection_chars: int = Field(
        default=1000,
        description="Number of characters to use for language detection",
    )
    language_confidence_threshold: float = Field(
        default=0.5,
        description="Minimum confidence threshold for language detection",
    )
    supported_languages: list[str] = Field(
        default=["ja", "en"],
        description="List of supported language codes",
    )

    # Phase 2: Chunking Configuration
    chunk_size: int = Field(
        default=512,
        description="Target chunk size in characters",
    )
    chunk_overlap: int = Field(
        default=50,
        description="Overlap between chunks in characters",
    )

    # Phase 2: Embedding Configuration
    embedding_model: str = Field(
        default="intfloat/multilingual-e5-base",
        description="Default embedding model (fallback for unsupported languages)",
    )
    embedding_model_ja: str = Field(
        default="cl-nagoya/ruri-v3-pt-30m",
        description="Embedding model for Japanese text",
    )
    embedding_model_en: str = Field(
        default="intfloat/multilingual-e5-base",
        description="Embedding model for English text",
    )
    embedding_batch_size: int = Field(
        default=32,
        description="Batch size for embedding generation",
    )
    embedding_dimension: int = Field(
        default=768,
        description="Dimension of embedding vectors (must match model)",
    )
    embedding_dimension_ja: int = Field(
        default=256,
        description="Dimension for Japanese embedding model (ruri-v3-pt-30m)",
    )
    embedding_dimension_en: int = Field(
        default=768,
        description="Dimension for English embedding model (multilingual-e5-base)",
    )

    # Phase 2: Pipeline Mode
    pipeline_mode: str = Field(
        default="full",
        description="Pipeline mode: 'simple' (text only) or 'full' (with embeddings)",
    )

    model_config = {
        "env_prefix": "",
        "case_sensitive": False,
    }


settings = Settings()
