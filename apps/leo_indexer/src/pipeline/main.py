"""Main entry point for LeoFS Indexing Worker."""

import asyncio
import logging
import signal
import sys
from typing import Optional

import structlog

from .config import settings

# Configure stdlib logging level based on settings
logging.basicConfig(
    format="%(message)s",
    stream=sys.stdout,
    level=getattr(logging, settings.log_level.upper(), logging.INFO),
)
from .consumer import NATSConsumer
from .task_index import TaskIndex
from .models import UploadEvent, IndexResult

# Configure structured logging
structlog.configure(
    processors=[
        structlog.stdlib.filter_by_level,
        structlog.stdlib.add_logger_name,
        structlog.stdlib.add_log_level,
        structlog.stdlib.PositionalArgumentsFormatter(),
        structlog.processors.TimeStamper(fmt="iso"),
        structlog.processors.StackInfoRenderer(),
        structlog.processors.format_exc_info,
        structlog.processors.UnicodeDecoder(),
        (
            structlog.dev.ConsoleRenderer()
            if sys.stdout.isatty()
            else structlog.processors.JSONRenderer()
        ),
    ],
    wrapper_class=structlog.stdlib.BoundLogger,
    context_class=dict,
    logger_factory=structlog.stdlib.LoggerFactory(),
    cache_logger_on_first_use=True,
)

logger = structlog.get_logger(__name__)


class IndexingWorker:
    """
    Main worker class that orchestrates NATS consumption and indexing.

    Lifecycle:
    1. Connect to NATS JetStream
    2. Subscribe to upload events
    3. Process each event with TaskIndex
    4. Handle graceful shutdown
    """

    def __init__(self):
        self.consumer: Optional[NATSConsumer] = None
        self.task_index: Optional[TaskIndex] = None
        self._shutdown_event = asyncio.Event()

        # Statistics
        self._processed_count = 0
        self._success_count = 0
        self._skip_count = 0
        self._error_count = 0

    async def start(self) -> None:
        """Start the indexing worker."""
        logger.info(
            "worker_starting",
            nats_url=settings.nats_url,
            subject=settings.nats_subject,
            vectors_bucket=settings.vectors_bucket,
        )

        # Initialize components
        self.task_index = TaskIndex()
        self.consumer = NATSConsumer()

        # Setup signal handlers
        self._setup_signal_handlers()

        try:
            # Connect to NATS
            await self.consumer.connect()

            # Start consuming
            logger.info("worker_started")
            await self.consumer.consume(self._handle_event)

        except asyncio.CancelledError:
            logger.info("worker_cancelled")
        finally:
            await self._shutdown()

    async def _handle_event(self, event: UploadEvent) -> None:
        """
        Handle a single upload event.

        Args:
            event: Parsed upload event from NATS
        """
        self._processed_count += 1

        try:
            result = await self.task_index.process(event)

            if result.status == "success":
                self._success_count += 1
                logger.info(
                    "event_processed",
                    status="success",
                    bucket=result.bucket,
                    key=result.key,
                    vectors_key=result.vectors_key,
                )
            elif result.status == "skipped":
                self._skip_count += 1
                logger.info(
                    "event_processed",
                    status="skipped",
                    bucket=result.bucket,
                    key=result.key,
                    reason=result.reason,
                )
            else:
                self._error_count += 1
                logger.warning(
                    "event_processed",
                    status=result.status,
                    bucket=result.bucket,
                    key=result.key,
                    error=result.error,
                )

        except Exception as e:
            self._error_count += 1
            logger.error(
                "event_processing_failed",
                bucket=event.bucket,
                key=event.key,
                error=str(e),
            )
            raise  # Re-raise for consumer retry/DLQ handling

    def _setup_signal_handlers(self) -> None:
        """Setup handlers for graceful shutdown."""
        loop = asyncio.get_running_loop()

        for sig in (signal.SIGTERM, signal.SIGINT):
            loop.add_signal_handler(
                sig,
                lambda s=sig: asyncio.create_task(self._signal_handler(s)),
            )

    async def _signal_handler(self, sig: signal.Signals) -> None:
        """Handle shutdown signal."""
        logger.info("shutdown_signal_received", signal=sig.name)
        self._shutdown_event.set()

        if self.consumer:
            await self.consumer.stop()

    async def _shutdown(self) -> None:
        """Perform graceful shutdown."""
        logger.info(
            "worker_shutting_down",
            processed=self._processed_count,
            success=self._success_count,
            skipped=self._skip_count,
            errors=self._error_count,
        )

        if self.consumer:
            await self.consumer.stop()

        logger.info("worker_stopped")


async def main() -> None:
    """Main entry point."""
    worker = IndexingWorker()
    await worker.start()


def run() -> None:
    """Run the worker (blocking)."""
    import traceback

    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        logger.info("interrupted_by_user")
    except Exception as e:
        logger.error(
            "fatal_error",
            error=str(e),
            error_type=type(e).__name__,
            traceback=traceback.format_exc(),
        )
        sys.exit(1)


if __name__ == "__main__":
    run()
