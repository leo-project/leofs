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
        # Use -name (long name) for IP addresses, -sname (short name) for hostnames without dots
        if echo "$NODENAME" | grep -q '@[0-9]'; then
            # Contains IP address, use -name
            sed -i "s/-sname .*/-name $NODENAME/" "$VM_ARGS_FILE"
            sed -i "s/-name .*/-name $NODENAME/" "$VM_ARGS_FILE"
        else
            # Use -sname for simple hostnames
            sed -i "s/-name .*/-sname $NODENAME/" "$VM_ARGS_FILE"
        fi
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

    # Add system/consistency settings to sys.config for leo_manager (master only)
    if [ -n "$SYS_CONFIG_FILE" ] && [ "$COMPONENT" = "leo_manager_0" ]; then
        # Extract consistency settings from config file
        NUM_REPLICAS=$(grep -E "^consistency.num_of_replicas\s*=" "$CONF_FILE" | sed 's/.*=\s*//' | tr -d ' ')
        WRITE=$(grep -E "^consistency.write\s*=" "$CONF_FILE" | sed 's/.*=\s*//' | tr -d ' ')
        READ=$(grep -E "^consistency.read\s*=" "$CONF_FILE" | sed 's/.*=\s*//' | tr -d ' ')
        DELETE=$(grep -E "^consistency.delete\s*=" "$CONF_FILE" | sed 's/.*=\s*//' | tr -d ' ')
        RACK_AWARE=$(grep -E "^consistency.rack_aware_replicas\s*=" "$CONF_FILE" | sed 's/.*=\s*//' | tr -d ' ')
        DC_ID=$(grep -E "^system.dc_id\s*=" "$CONF_FILE" | sed 's/.*=\s*//' | tr -d ' ')
        CLUSTER_ID=$(grep -E "^system.cluster_id\s*=" "$CONF_FILE" | sed 's/.*=\s*//' | tr -d ' ')

        # Set defaults if not found
        NUM_REPLICAS=${NUM_REPLICAS:-1}
        WRITE=${WRITE:-1}
        READ=${READ:-1}
        DELETE=${DELETE:-1}
        RACK_AWARE=${RACK_AWARE:-0}
        DC_ID=${DC_ID:-dc_1}
        CLUSTER_ID=${CLUSTER_ID:-leofs_1}

        echo "Setting consistency: n=$NUM_REPLICAS, w=$WRITE, r=$READ, d=$DELETE"

        # Build system config tuple
        SYSTEM_CONFIG="{system, [{dc_id, $DC_ID}, {cluster_id, $CLUSTER_ID}, {n, $NUM_REPLICAS}, {w, $WRITE}, {r, $READ}, {d, $DELETE}, {bit_of_ring, 128}, {num_of_rack_replicas, $RACK_AWARE}]}"

        # Add system config to leo_manager section (before port_cui)
        sed -i "s/{port_cui, \([0-9]*\)}/$SYSTEM_CONFIG, {port_cui, \1}/" "$SYS_CONFIG_FILE"
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

    # Update RPC listen port for storage/gateway nodes (critical for running multiple nodes)
    if [ -n "$SYS_CONFIG_FILE" ] && ([ "$BIN_NAME" = "leo_storage" ] || [ "$BIN_NAME" = "leo_gateway" ]); then
        RPC_PORT=$(grep -E "^rpc.server.listen_port\s*=" "$CONF_FILE" | sed 's/.*=\s*//' | tr -d ' ')
        if [ -n "$RPC_PORT" ]; then
            echo "Setting RPC listen port to: $RPC_PORT"
            sed -i "s/{listen_port, [0-9]*}/{listen_port, $RPC_PORT}/" "$SYS_CONFIG_FILE"
        fi
    fi

    # Add internal_network settings for leo_gateway
    if [ -n "$SYS_CONFIG_FILE" ] && [ "$BIN_NAME" = "leo_gateway" ]; then
        INTERNAL_NETWORK_ENABLED=$(grep -E "^internal_network\.enabled\s*=" "$CONF_FILE" | sed 's/.*=\s*//' | tr -d ' ')
        INTERNAL_NETWORK_CIDRS=$(grep -E "^internal_network\.cidrs\s*=" "$CONF_FILE" | sed 's/.*=\s*//')

        if [ -n "$INTERNAL_NETWORK_ENABLED" ]; then
            echo "Setting internal_network.enabled to: $INTERNAL_NETWORK_ENABLED"

            # Convert CIDRs to Erlang format: "10.0.0.0/8, 172.16.0.0/12" -> [{{10,0,0,0}, 8}, {{172,16,0,0}, 12}]
            ERLANG_CIDRS="[]"
            if [ -n "$INTERNAL_NETWORK_CIDRS" ]; then
                ERLANG_CIDRS="["
                FIRST=1
                # Process each CIDR
                for CIDR in $(echo "$INTERNAL_NETWORK_CIDRS" | tr ',' ' '); do
                    CIDR=$(echo "$CIDR" | tr -d ' ')
                    if [ -n "$CIDR" ]; then
                        IP=$(echo "$CIDR" | cut -d'/' -f1)
                        MASK=$(echo "$CIDR" | cut -d'/' -f2)
                        # Convert IP to tuple format: 10.0.0.0 -> {10,0,0,0}
                        IP_TUPLE=$(echo "$IP" | sed 's/\./,/g')
                        if [ "$FIRST" = "1" ]; then
                            ERLANG_CIDRS="${ERLANG_CIDRS}{{${IP_TUPLE}}, ${MASK}}"
                            FIRST=0
                        else
                            ERLANG_CIDRS="${ERLANG_CIDRS}, {{${IP_TUPLE}}, ${MASK}}"
                        fi
                    fi
                done
                ERLANG_CIDRS="${ERLANG_CIDRS}]"
            fi

            echo "Setting internal_network.cidrs to: $ERLANG_CIDRS"

            # Add internal_network settings to leo_gateway section in sys.config
            # Insert before the closing of leo_gateway tuple
            sed -i "s/{managers, \(\[.*\]\)}/{managers, \1}, {internal_network_enabled, $INTERNAL_NETWORK_ENABLED}, {internal_network_cidrs, $ERLANG_CIDRS}/" "$SYS_CONFIG_FILE"
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
