"""Configuration management using pydantic-settings."""

from pydantic_settings import BaseSettings
from pydantic import Field


class Settings(BaseSettings):
    """Application settings loaded from environment variables."""

    # NATS Configuration
    nats_url: str = Field(
        default="nats://localhost:4222",
        description="NATS server URL"
    )
    nats_stream: str = Field(
        default="LEOFS_EVENTS",
        description="JetStream stream name"
    )
    nats_subject: str = Field(
        default="leofs.events.upload",
        description="Subject to subscribe to"
    )
    nats_consumer: str = Field(
        default="leo-indexer",
        description="Durable consumer name"
    )
    nats_max_deliver: int = Field(
        default=3,
        description="Max delivery attempts before DLQ"
    )
    nats_ack_wait: int = Field(
        default=30,
        description="ACK wait time in seconds"
    )

    # LeoFS S3 Configuration
    leofs_endpoint: str = Field(
        default="http://localhost:8080",
        description="LeoFS S3 API endpoint"
    )
    leofs_access_key: str = Field(
        default="",
        description="LeoFS access key"
    )
    leofs_secret_key: str = Field(
        default="",
        description="LeoFS secret key"
    )

    # Storage Configuration
    temp_dir: str = Field(
        default="/tmp/lance",
        description="Temporary directory for LanceDB"
    )
    vectors_bucket: str = Field(
        default="",
        description="Bucket for vector storage (empty = same as source bucket)"
    )

    # DLQ Configuration
    dlq_subject: str = Field(
        default="leofs.events.dlq",
        description="Dead Letter Queue subject"
    )

    # Logging
    log_level: str = Field(
        default="INFO",
        description="Log level (DEBUG, INFO, WARNING, ERROR)"
    )

    model_config = {
        "env_prefix": "",
        "case_sensitive": False,
    }


settings = Settings()
