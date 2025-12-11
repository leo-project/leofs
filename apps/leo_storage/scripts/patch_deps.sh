#!/bin/sh
#
# Patch dependencies for compatibility with newer build tools
#

BASEDIR=$(cd "$(dirname "$0")/.." && pwd)
ELEVELDB_CSRC="${BASEDIR}/_build/default/lib/eleveldb/c_src"
ELEVELDB_MAKEFILE="${ELEVELDB_CSRC}/Makefile"

# Clean up snappy build directory if it exists (prevents "mkdir: build: File exists" error)
if [ -d "${ELEVELDB_CSRC}/snappy-1.1.9" ]; then
    echo "Cleaning up existing snappy build directory..."
    rm -rf "${ELEVELDB_CSRC}/snappy-1.1.9"
fi

# Patch eleveldb Makefile for CMake 3.27+ compatibility
if [ -f "$ELEVELDB_MAKEFILE" ]; then
    if ! grep -q "CMAKE_POLICY_VERSION_MINIMUM" "$ELEVELDB_MAKEFILE"; then
        echo "Patching eleveldb Makefile for CMake compatibility..."
        TMPFILE=$(mktemp)
        sed 's/-D CMAKE_INSTALL_PREFIX=\$(BASEDIR)\/system \\$/-D CMAKE_INSTALL_PREFIX=$(BASEDIR)\/system \\\
	         -D CMAKE_POLICY_VERSION_MINIMUM=3.5 \\/' "$ELEVELDB_MAKEFILE" > "$TMPFILE"
        mv "$TMPFILE" "$ELEVELDB_MAKEFILE"
        echo "Patched eleveldb Makefile successfully."
    fi
fi

exit 0
