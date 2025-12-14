#!/bin/bash
#======================================================================
#
# Leo Gateway - Self-signed SSL Certificate Generator
#
# Copyright (c) 2012-2018 Rakuten, Inc.
# Copyright (c) 2019-2025 Lions Data, Ltd.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
#======================================================================

set -e

# Default values
CERT_DIR="${1:-./etc}"
CERT_FILE="${CERT_DIR}/server_cert.pem"
KEY_FILE="${CERT_DIR}/server_key.pem"
DAYS=365
KEY_SIZE=2048
CN="${2:-localhost}"

usage() {
    echo "Usage: $0 [CERT_DIR] [COMMON_NAME]"
    echo ""
    echo "Generate self-signed SSL certificate for Leo Gateway."
    echo ""
    echo "Arguments:"
    echo "  CERT_DIR     Directory to store certificates (default: ./etc)"
    echo "  COMMON_NAME  Common Name for the certificate (default: localhost)"
    echo ""
    echo "Examples:"
    echo "  $0                          # Create in ./etc with CN=localhost"
    echo "  $0 /path/to/certs           # Create in /path/to/certs with CN=localhost"
    echo "  $0 ./etc gateway.example.com # Create in ./etc with CN=gateway.example.com"
    exit 1
}

# Show help
if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    usage
fi

echo "=========================================="
echo " Leo Gateway SSL Certificate Generator"
echo "=========================================="
echo ""

# Create certificate directory if it doesn't exist
if [ ! -d "${CERT_DIR}" ]; then
    echo "Creating certificate directory: ${CERT_DIR}"
    mkdir -p "${CERT_DIR}"
fi

# Check if certificates already exist
if [ -f "${CERT_FILE}" ] || [ -f "${KEY_FILE}" ]; then
    echo "Warning: Certificate files already exist!"
    echo "  - ${CERT_FILE}"
    echo "  - ${KEY_FILE}"
    read -p "Do you want to overwrite them? (y/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted."
        exit 0
    fi
fi

echo "Generating self-signed SSL certificate..."
echo "  Directory:   ${CERT_DIR}"
echo "  Common Name: ${CN}"
echo "  Key Size:    ${KEY_SIZE} bits"
echo "  Valid Days:  ${DAYS}"
echo ""

# Generate self-signed certificate
openssl req -x509 \
    -newkey rsa:${KEY_SIZE} \
    -keyout "${KEY_FILE}" \
    -out "${CERT_FILE}" \
    -days ${DAYS} \
    -nodes \
    -subj "/CN=${CN}" \
    2>&1

# Set appropriate permissions
chmod 644 "${CERT_FILE}"
chmod 600 "${KEY_FILE}"

echo ""
echo "=========================================="
echo " Certificate generated successfully!"
echo "=========================================="
echo ""
echo "Certificate file: ${CERT_FILE}"
echo "Private key file: ${KEY_FILE}"
echo ""
echo "Note: This is a self-signed certificate for development/testing."
echo "      For production, please use a certificate from a trusted CA."
