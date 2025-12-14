#!/bin/bash
set -e

COMPONENT=${COMPONENT:-leo_manager_0}

# Determine the binary name and config file
case "$COMPONENT" in
    leo_manager_0|leo_manager_1)
        BIN_NAME="leo_manager"
        CONF_FILE="etc/leo_manager.conf"
        ;;
    leo_storage)
        BIN_NAME="leo_storage"
        CONF_FILE="etc/leo_storage.conf"
        ;;
    leo_gateway)
        BIN_NAME="leo_gateway"
        CONF_FILE="etc/leo_gateway.conf"
        ;;
    *)
        echo "Unknown component: $COMPONENT"
        exit 1
        ;;
esac

# Extract nodename and cookie from config file and update vm.args
if [ -f "$CONF_FILE" ]; then
    NODENAME=$(grep -E "^nodename\s*=" "$CONF_FILE" | sed 's/.*=\s*//' | tr -d ' ')
    COOKIE=$(grep -E "^distributed_cookie\s*=" "$CONF_FILE" | sed 's/.*=\s*//' | tr -d ' ')
    BIND_ADDR=$(grep -E "^console.bind_address\s*=" "$CONF_FILE" | sed 's/.*=\s*//' | tr -d ' ')

    VM_ARGS_FILE=$(find releases -name "vm.args" | head -1)
    SYS_CONFIG_FILE=$(find releases -name "sys.config" | head -1)

    if [ -n "$NODENAME" ] && [ -n "$VM_ARGS_FILE" ]; then
        echo "Setting nodename to: $NODENAME"
        # Use -sname (short name) instead of -name (requires FQDN) for Docker networking
        sed -i "s/-name .*/-sname $NODENAME/" "$VM_ARGS_FILE"
    fi

    if [ -n "$COOKIE" ] && [ -n "$VM_ARGS_FILE" ]; then
        echo "Setting cookie to: $COOKIE"
        sed -i "s/-setcookie .*/-setcookie $COOKIE/" "$VM_ARGS_FILE"
    fi

    # Add bind_address to sys.config for leo_manager
    if [ -n "$BIND_ADDR" ] && [ -n "$SYS_CONFIG_FILE" ] && [ "$BIN_NAME" = "leo_manager" ]; then
        echo "Setting bind_address to: $BIND_ADDR"
        # Add bind_address to leo_manager config (before port_cui)
        sed -i "s/{port_cui, \([0-9]*\)}/{bind_address, \"$BIND_ADDR\"}, {port_cui, \1}/" "$SYS_CONFIG_FILE"
    fi

    # Update manager addresses for storage/gateway from config file
    if [ -n "$SYS_CONFIG_FILE" ] && ([ "$BIN_NAME" = "leo_storage" ] || [ "$BIN_NAME" = "leo_gateway" ]); then
        MANAGERS=$(grep -E "^managers\s*=" "$CONF_FILE" | sed 's/.*=\s*//' | tr -d ' ')
        if [ -n "$MANAGERS" ]; then
            echo "Setting managers to: $MANAGERS"
            # Convert config format [a@b, c@d] to Erlang format ['a@b', 'c@d']
            ERLANG_MANAGERS=$(echo "$MANAGERS" | sed "s/\[/['/g" | sed "s/\]/']/" | sed "s/,/','/g")
            sed -i "s/{managers, \[.*\]}/{managers, $ERLANG_MANAGERS}/" "$SYS_CONFIG_FILE"
        fi
    fi
fi

# Wait for dependencies
if [ -n "$WAIT_FOR_HOST" ] && [ -n "$WAIT_FOR_PORT" ]; then
    echo "Waiting for $WAIT_FOR_HOST:$WAIT_FOR_PORT..."
    while ! nc -z "$WAIT_FOR_HOST" "$WAIT_FOR_PORT" 2>/dev/null; do
        sleep 1
    done
    echo "$WAIT_FOR_HOST:$WAIT_FOR_PORT is available"
fi

# Additional wait time for service to be fully ready
if [ -n "$WAIT_SECONDS" ]; then
    echo "Waiting additional $WAIT_SECONDS seconds..."
    sleep "$WAIT_SECONDS"
fi

echo "Starting $COMPONENT..."

# Start the node in foreground mode
exec ./bin/${BIN_NAME} foreground
