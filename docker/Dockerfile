# =============================================================================
# LeoFS Runtime Image
# Requires: leofs-builder image (build with Dockerfile.builder first)
# =============================================================================
FROM leofs-builder:latest AS builder

FROM ubuntu:24.04

ARG COMPONENT

# Timezone
ENV TZ=Asia/Tokyo

# Fix apt proxy/mirror issues
COPY docker/99fixbadproxy /etc/apt/apt.conf.d/

# Install runtime dependencies
RUN rm -rf /var/lib/apt/lists/* \
    && apt-get clean \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        netcat-openbsd \
        libncurses6 \
        libssl3 \
    && rm -rf /var/lib/apt/lists/*

# Copy built package from builder
COPY --from=builder /build/package/${COMPONENT} /opt/${COMPONENT}
COPY docker/entrypoint.sh /opt/entrypoint.sh

RUN chmod +x /opt/entrypoint.sh \
    && chmod +x /opt/${COMPONENT}/bin/*

WORKDIR /opt/${COMPONENT}

ENV COMPONENT=${COMPONENT}

ENTRYPOINT ["/opt/entrypoint.sh"]
