"""Pytest configuration and fixtures."""

import pytest
import asyncio


@pytest.fixture(scope="session")
def event_loop():
    """Create an event loop for async tests."""
    loop = asyncio.new_event_loop()
    yield loop
    loop.close()


@pytest.fixture
def sample_upload_event():
    """Sample upload event data."""
    return {
        "event": "object_created",
        "bucket": "test-bucket",
        "key": "documents/test.txt",
        "timestamp": 1734567890,
        "node_id": None,
    }


@pytest.fixture
def sample_text_content():
    """Sample text file content."""
    return "Hello, this is a test document for LeoFS indexing."
