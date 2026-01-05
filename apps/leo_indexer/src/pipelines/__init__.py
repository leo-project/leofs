"""Pipeline definitions for leo_indexer.

This module provides different pipeline implementations:
- SimplePipeline: Phase 1 - Text storage only
- FullPipeline: Phase 2 - Language detection, chunking, embeddings
"""

from .base import BasePipeline, PipelineContext
from .simple import SimplePipeline
from .full import FullPipeline

__all__ = [
    "BasePipeline",
    "PipelineContext",
    "SimplePipeline",
    "FullPipeline",
]
