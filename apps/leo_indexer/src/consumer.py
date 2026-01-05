"""NATS JetStream Consumer for LeoFS upload events."""

import asyncio
import json
from typing import Callable, Awaitable, Optional

import nats
from nats.js.api import (
    ConsumerConfig,
    AckPolicy,
    DeliverPolicy,
    StreamConfig,
    RetentionPolicy,
)
from nats.js.errors import NotFoundError as NatsNotFoundError
from nats.errors import TimeoutError as NatsTimeoutError
import structlog

from .config import settings
from .models import UploadEvent
from .exceptions import RetryableError, PermanentError

logger = structlog.get_logger(__name__)

# Type alias for message handler
MessageHandler = Callable[[UploadEvent], Awaitable[None]]


class NATSConsumer:
    """
    NATS JetStream Pull Consumer for processing LeoFS upload events.

    Features:
    - Durable consumer with explicit ACK
    - Automatic stream/consumer creation
    - Retry with NAK delay
    - DLQ forwarding for permanent failures
    """

    def __init__(
        self,
        nats_url: str = settings.nats_url,
        stream_name: str = settings.nats_stream,
        subject: str = settings.nats_subject,
        consumer_name: str = settings.nats_consumer,
        max_deliver: int = settings.nats_max_deliver,
        ack_wait: int = settings.nats_ack_wait,
        dlq_subject: str = settings.dlq_subject,
    ):
        self.nats_url = nats_url
        self.stream_name = stream_name
        self.subject = subject
        self.consumer_name = consumer_name
        self.max_deliver = max_deliver
        self.ack_wait = ack_wait
        self.dlq_subject = dlq_subject

        self._nc: Optional[nats.NATS] = None
        self._js: Optional[nats.js.JetStreamContext] = None
        self._subscription = None
        self._running = False

    async def connect(self) -> None:
        """Connect to NATS and setup JetStream."""
        logger.info("connecting_to_nats", url=self.nats_url)

        self._nc = await nats.connect(self.nats_url)
        self._js = self._nc.jetstream()

        # Ensure stream exists
        await self._ensure_stream()

        # Create pull subscription
        self._subscription = await self._js.pull_subscribe(
            self.subject,
            durable=self.consumer_name,
            config=ConsumerConfig(
                ack_policy=AckPolicy.EXPLICIT,
                deliver_policy=DeliverPolicy.ALL,
                max_deliver=self.max_deliver,
                ack_wait=self.ack_wait,
            ),
        )

        logger.info(
            "connected_to_nats",
            stream=self.stream_name,
            subject=self.subject,
            consumer=self.consumer_name,
        )

    async def _ensure_stream(self) -> None:
        """Create stream if it doesn't exist."""
        try:
            await self._js.stream_info(self.stream_name)
            logger.debug("stream_exists", stream=self.stream_name)
        except NatsNotFoundError:
            logger.info("creating_stream", stream=self.stream_name)
            await self._js.add_stream(
                config=StreamConfig(
                    name=self.stream_name,
                    subjects=["leofs.events.>"],
                    retention=RetentionPolicy.WORK_QUEUE,
                )
            )

    async def consume(
        self,
        handler: MessageHandler,
        batch_size: int = 10,
        fetch_timeout: float = 5.0,
    ) -> None:
        """
        Start consuming messages and processing with the handler.

        Args:
            handler: Async function to process UploadEvent
            batch_size: Number of messages to fetch at once
            fetch_timeout: Timeout for fetch operation in seconds
        """
        self._running = True
        logger.info("starting_consumer", batch_size=batch_size)

        while self._running:
            try:
                msgs = await self._subscription.fetch(
                    batch=batch_size,
                    timeout=fetch_timeout,
                )

                for msg in msgs:
                    await self._process_message(msg, handler)

            except NatsTimeoutError:
                # No messages available, continue polling
                continue
            except Exception as e:
                logger.error("consumer_error", error=str(e))
                await asyncio.sleep(1)  # Brief pause before retry

    async def _process_message(
        self,
        msg,
        handler: MessageHandler,
    ) -> None:
        """Process a single message with error handling."""
        msg_id = msg.headers.get("Nats-Msg-Id", "unknown") if msg.headers else "unknown"

        try:
            # Parse message
            data = json.loads(msg.data.decode("utf-8"))
            event = UploadEvent(**data)

            logger.info(
                "processing_message",
                msg_id=msg_id,
                bucket=event.bucket,
                key=event.key,
            )

            # Process with handler
            await handler(event)

            # Acknowledge success
            await msg.ack()
            logger.info("message_acked", msg_id=msg_id)

        except json.JSONDecodeError as e:
            logger.error("invalid_json", msg_id=msg_id, error=str(e))
            await self._send_to_dlq(msg, f"Invalid JSON: {e}")
            await msg.ack()  # Remove from queue

        except PermanentError as e:
            logger.error("permanent_error", msg_id=msg_id, error=str(e))
            await self._send_to_dlq(msg, str(e))
            await msg.ack()  # Remove from queue

        except RetryableError as e:
            logger.warning("retryable_error", msg_id=msg_id, error=str(e))
            await msg.nak(delay=5)  # Retry after 5 seconds

        except Exception as e:
            logger.error("unexpected_error", msg_id=msg_id, error=str(e))
            # Check delivery count
            metadata = msg.metadata
            if metadata and metadata.num_delivered >= self.max_deliver:
                logger.error("max_retries_exceeded", msg_id=msg_id)
                await self._send_to_dlq(msg, f"Max retries exceeded: {e}")
                await msg.ack()
            else:
                await msg.nak(delay=5)

    async def _send_to_dlq(self, msg, error_reason: str) -> None:
        """Send failed message to Dead Letter Queue."""
        try:
            headers = {
                "X-Error-Reason": error_reason,
                "X-Original-Subject": self.subject,
            }
            await self._js.publish(
                self.dlq_subject,
                msg.data,
                headers=headers,
            )
            logger.info(
                "message_sent_to_dlq",
                dlq_subject=self.dlq_subject,
                reason=error_reason,
            )
        except Exception as e:
            logger.error("dlq_send_failed", error=str(e))

    async def stop(self) -> None:
        """Stop the consumer gracefully."""
        self._running = False
        if self._subscription:
            await self._subscription.unsubscribe()
        if self._nc:
            await self._nc.drain()
        logger.info("consumer_stopped")

    async def __aenter__(self):
        """Async context manager entry."""
        await self.connect()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Async context manager exit."""
        await self.stop()
