use std::env;
use std::sync::atomic::{AtomicU64, Ordering};
use tokio::net::UdpSocket;
use serde::Serialize;
use log::{info, error, warn, debug};
use anyhow::Result;

// Global counters for statistics
static MSG_RECEIVED: AtomicU64 = AtomicU64::new(0);
static MSG_PUBLISHED: AtomicU64 = AtomicU64::new(0);
static MSG_FAILED: AtomicU64 = AtomicU64::new(0);

// Key prefix to filter out (internal vector storage files)
const FILTER_KEY_PREFIX_VECTORS: &str = ".vectors/";

#[derive(Serialize, Debug)]
struct UploadEvent {
    event: String,
    bucket: String,
    key: String,
    timestamp: i64,
    node_id: Option<String>,
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logger with default level INFO if RUST_LOG not set
    if env::var("RUST_LOG").is_err() {
        env::set_var("RUST_LOG", "info");
    }
    env_logger::init();

    info!("========================================");
    info!("  LeoFS NATS Bridge (leo_nats_bridge)");
    info!("========================================");

    // Configuration from Environment Variables
    let nats_url = env::var("NATS_URL").unwrap_or_else(|_| "nats://localhost:4222".to_string());
    let udp_port = env::var("BRIDGE_PORT").unwrap_or_else(|_| "5000".to_string());
    let udp_addr = format!("0.0.0.0:{}", udp_port);

    info!("Configuration:");
    info!("  NATS_URL:    {}", nats_url);
    info!("  BRIDGE_PORT: {}", udp_port);

    // Connect to NATS
    info!("Connecting to NATS at {}...", nats_url);
    let nats_client = async_nats::connect(&nats_url).await?;
    info!("Connected to NATS successfully");

    // Create JetStream Context
    let jetstream = async_nats::jetstream::new(nats_client);

    // Bind UDP Socket
    let socket = UdpSocket::bind(&udp_addr).await?;
    info!("UDP socket bound to {}", udp_addr);
    info!("----------------------------------------");
    info!("Waiting for messages from leo_gateway...");
    info!("----------------------------------------");

    let mut buf = [0; 65535]; // Max UDP size

    loop {
        match socket.recv_from(&mut buf).await {
            Ok((size, peer)) => {
                let count = MSG_RECEIVED.fetch_add(1, Ordering::Relaxed) + 1;
                let data = &buf[..size];
                match String::from_utf8(data.to_vec()) {
                    Ok(msg) => {
                        info!("[UDP #{:>6}] Received from {}: {} bytes", count, peer, size);
                        debug!("  Raw message: {}", msg.trim());
                        let js_clone = jetstream.clone();
                        tokio::spawn(async move {
                            if let Err(e) = process_message(js_clone, msg).await {
                                MSG_FAILED.fetch_add(1, Ordering::Relaxed);
                                error!("Failed to process message: {}", e);
                            }
                        });
                    }
                    Err(e) => {
                        warn!("[UDP #{:>6}] Invalid UTF-8 data from {}: {}", count, peer, e);
                    }
                }
            }
            Err(e) => {
                error!("UDP Receive error: {}", e);
            }
        }
    }
}

async fn process_message(jetstream: async_nats::jetstream::Context, msg: String) -> Result<()> {
    // Expected format: <BucketName>|<ObjectKey>
    // msg might contain newlines, trim it.
    let trimmed_msg = msg.trim();

    // Split once by '|'
    if let Some((bucket, key)) = trimmed_msg.split_once('|') {
        if bucket.is_empty() || key.is_empty() {
             warn!("  -> Invalid format (empty bucket or key): {}", trimmed_msg);
             return Ok(());
        }

        // Filter out .vectors/* files (internal vector storage, no need to notify)
        if key.starts_with(FILTER_KEY_PREFIX_VECTORS) {
            debug!("  -> Filtered: skipping vector file key={}", key);
            return Ok(());
        }

        let event = UploadEvent {
            event: "object_created".to_string(),
            bucket: bucket.to_string(),
            key: key.to_string(),
            timestamp: chrono::Utc::now().timestamp(),
            node_id: None,
        };

        let payload = serde_json::to_vec(&event)?;
        let subject = "leofs.events.upload";

        info!("  -> Parsed: bucket={}, key={}", bucket, key);
        debug!("  -> Event: {:?}", event);

        // Publish to JetStream (async-nats 0.44+ requires two awaits)
        match jetstream.publish(subject, payload.into()).await {
            Ok(ack_future) => {
                match ack_future.await {
                    Ok(ack) => {
                        let pub_count = MSG_PUBLISHED.fetch_add(1, Ordering::Relaxed) + 1;
                        info!("  -> Published to NATS [subject={}] (total: {})", subject, pub_count);
                        debug!("  -> Ack: {:?}", ack);
                    }
                    Err(e) => {
                        let fail_count = MSG_FAILED.fetch_add(1, Ordering::Relaxed) + 1;
                        error!("  -> NATS Ack FAILED: {} (total failures: {})", e, fail_count);
                        return Err(e.into());
                    }
                }
            }
            Err(e) => {
                let fail_count = MSG_FAILED.fetch_add(1, Ordering::Relaxed) + 1;
                error!("  -> NATS Publish FAILED: {} (total failures: {})", e, fail_count);
                return Err(e.into());
            }
        }
    } else {
        warn!("  -> Invalid message format (expected 'bucket|key'): {}", trimmed_msg);
    }

    Ok(())
}
