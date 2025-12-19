# Build Stage
FROM rust:latest as builder

WORKDIR /usr/src/app
COPY . .

RUN cargo build --release

# Runtime Stage
FROM debian:bookworm-slim

WORKDIR /app
COPY --from=builder /usr/src/app/target/release/leo_nats_bridge /usr/local/bin/leo_nats_bridge

CMD ["leo_nats_bridge"]
