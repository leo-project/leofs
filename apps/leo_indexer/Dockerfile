# LeoFS Indexing Worker
# Multi-stage build for minimal image size

# Build stage
FROM python:3.14-slim AS builder

WORKDIR /app

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements and install dependencies
COPY requirements.txt .
RUN pip install --no-cache-dir --user -r requirements.txt

# Runtime stage
FROM python:3.14-slim

WORKDIR /app

# Install runtime dependencies (for tar operations)
RUN apt-get update && apt-get install -y --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

# Copy installed packages from builder
COPY --from=builder /root/.local /root/.local

# Ensure scripts in .local are usable
ENV PATH=/root/.local/bin:$PATH

# Copy application code
COPY src/ ./src/

# Create temp directory for LanceDB
RUN mkdir -p /tmp/lance

# Set Python path
ENV PYTHONPATH=/app

# Health check (basic process check)
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD pgrep -f "python -m src.pipeline.main" || exit 1

# Run the worker
CMD ["python", "-m", "src.pipeline.main"]
