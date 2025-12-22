"""LeoFS Indexing Worker.

Processes upload events from NATS JetStream and creates LanceDB indexes.
"""

__version__ = "0.2.0"

from .config import settings
from .models import UploadEvent, IndexResult
from .exceptions import IndexingError, RetryableError, PermanentError

__all__ = [
    "settings",
    "UploadEvent",
    "IndexResult",
    "IndexingError",
    "RetryableError",
    "PermanentError",
]
