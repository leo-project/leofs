#!/bin/sh
#
# Patch dependencies for compatibility with newer build tools
#

BASEDIR=$(cd "$(dirname "$0")/.." && pwd)
ELEVELDB_MAKEFILE="${BASEDIR}/_build/default/lib/eleveldb/c_src/Makefile"
LEO_MCERL_DIR="${BASEDIR}/_build/default/lib/leo_mcerl/c_src"
LEO_DCERL_DIR="${BASEDIR}/_build/default/lib/leo_dcerl/c_src"

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

# Build libcutil for leo_mcerl if needed
if [ -d "$LEO_MCERL_DIR" ]; then
    if [ ! -f "$LEO_MCERL_DIR/libcutil/build/src/libcutil.a" ]; then
        echo "Building libcutil for leo_mcerl..."
        (cd "$LEO_MCERL_DIR" && ./build_deps.sh)
        echo "Built libcutil for leo_mcerl successfully."
    fi
fi

# Build libcutil for leo_dcerl if needed
if [ -d "$LEO_DCERL_DIR" ]; then
    if [ ! -f "$LEO_DCERL_DIR/libcutil/build/src/libcutil.a" ]; then
        echo "Building libcutil for leo_dcerl..."
        (cd "$LEO_DCERL_DIR" && ./build_deps.sh)
        echo "Built libcutil for leo_dcerl successfully."
    fi
fi

exit 0
