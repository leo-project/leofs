"""Custom exceptions for the indexing worker."""


class IndexingError(Exception):
    """Base exception for indexing errors."""

    pass


class RetryableError(IndexingError):
    """
    Error that can be retried (temporary failures).

    Examples:
    - Network timeout
    - Service temporarily unavailable
    - Rate limiting
    """

    pass


class PermanentError(IndexingError):
    """
    Error that should not be retried (permanent failures).

    Examples:
    - Invalid data format
    - Object not found (404)
    - Permission denied
    - Unsupported file type
    """

    pass


class S3Error(IndexingError):
    """Error related to S3 operations."""

    pass


class LanceDBError(IndexingError):
    """Error related to LanceDB operations."""

    pass
