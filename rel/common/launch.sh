#!/bin/sh
# -*- tab-width:4;indent-tabs-mode:nil -*-
# ex: ts=4 sw=4 et

# Identify the script name
SCRIPT=`basename "$0"`

RUNNER_SCRIPT_DIR=$(cd ${0%/*} && pwd)
RUNNER_BASE_DIR=${RUNNER_SCRIPT_DIR%/*}
NODE_TYPE=$(basename "$RUNNER_BASE_DIR")

# Load "GLOBAL_CONFIG" option from here (when it's "yes", it means that environment
# file and config file will be picked from /etc/leofs/$NODE_TYPE instead)
[ -f /etc/leofs/leofs.conf ] && . /etc/leofs/leofs.conf
GLOBAL_CONFIG="$(echo $GLOBAL_CONFIG | tr '[A-Z]' '[a-z]')"


# Check if we need to pick alternate set of config files. Compatible only with global config.
if [ "$NODE_EXTRA_NAME" != "" ]
then
    CONFIG_DIR="${NODE_TYPE}_${NODE_EXTRA_NAME}"
else
    CONFIG_DIR="$NODE_TYPE"
    # Define the variable, even if it wasn't before. We rely on it when parsing "ps" output for "stop" command
    NODE_EXTRA_NAME=
    export NODE_EXTRA_NAME
fi


# Source environment script to allow redefining all other variables
if [ "$GLOBAL_CONFIG" = "yes" ]
then
    CONFIG_PATH=/etc/leofs/$CONFIG_DIR/$SCRIPT.environment
else
    CONFIG_PATH=$RUNNER_BASE_DIR/etc/$SCRIPT.environment
fi

[ -f $CONFIG_PATH ] && . $CONFIG_PATH

if [ "$GLOBAL_CONFIG" = "yes" ]
then
    RUNNER_ETC_DIR=${RUNNER_ETC_DIR:-/etc/leofs/$CONFIG_DIR}
else
    RUNNER_ETC_DIR=${RUNNER_ETC_DIR:-$RUNNER_BASE_DIR/etc}
fi

RUNNER_SCHEMA_DIR=${RUNNER_SCHEMA_DIR:-$RUNNER_BASE_DIR/etc}
RUNNER_LOG_DIR=${RUNNER_LOG_DIR:-$RUNNER_BASE_DIR/log}
# Note the trailing slash on $PIPE_DIR/
PIPE_DIR=/tmp/$RUNNER_BASE_DIR/
RUNNER_USER=${RUNNER_USER:-leofs}

# Make sure this script is running as the appropriate user
if [ ! -z "$RUNNER_USER" ] && [ `whoami` != "$RUNNER_USER" ]; then
    exec sudo -H -u $RUNNER_USER "$RUNNER_SCRIPT_DIR/$SCRIPT" "$@"
fi

# Make sure CWD is set to runner base dir
cd $RUNNER_BASE_DIR

# Make sure log directory exists
mkdir -p $RUNNER_LOG_DIR

# Try to increase open files limit. Workaround for Ubuntu (and maybe others) default
# sudo settings which don't use PAM to set limits automatically. Error is silenced because
# there are known cases when limit can't be changed (e.g. launching managers under systemd)
ulimit -n ${MAX_OPEN_FILES:-65535} 2> /dev/null

help () {
    echo "Usage: $SCRIPT [-type leo_manager|leo_gateway|leo_storage] {start|stop|restart|foreground_start|reboot|ping|console|console_clean|attach|remote_console}"
    echo "Script type is picked from its name, but can be overriden with -type option"
    echo "leo_manager additionally supports commands {dump-mnesia-data|load-mnesia-data}"
}

case "$1" in
    -type)
        case "$2" in
            leo_manager|leo_gateway|leo_storage)
                NODE_TYPE=$2
                shift 2
                ;;
            *)
                help
                exit 1
                ;;
        esac
        ;;
    *)
        case "$SCRIPT" in
            leo_manager|leo_gateway|leo_storage)
                NODE_TYPE=$SCRIPT
                ;;
            *)
                echo "Script name (currently: $SCRIPT) should be one of: leo_manager, leo_gateway, leo_manager"
                echo "Or '-type leo_manger|leo_gateway|leo_storage' has to be supplied as the first option"
                exit 1
                ;;
        esac
        ;;
esac

case "$NODE_TYPE" in
    leo_storage)
        ERLEXEC_MODE=minimal
        ;;
    leo_gateway|leo_manager)
        ERLEXEC_MODE=embedded
        ;;
esac


# Parse out release and erts info
START_ERL=`cat $RUNNER_BASE_DIR/releases/start_erl.data`
ERTS_VSN=${START_ERL% *}
APP_VSN=${START_ERL#* }

# Use releases/VSN/vm.args if it exists otherwise use etc/vm.args
# if [ -e "$RUNNER_BASE_DIR/releases/$APP_VSN/vm.args" ]; then
#     VMARGS_PATH="$RUNNER_BASE_DIR/releases/$APP_VSN/vm.args"
# else
#     VMARGS_PATH="$RUNNER_ETC_DIR/vm.args"
# fi
VMARGS_PATH="$RUNNER_ETC_DIR/vm.args"

# Use releases/VSN/sys.config if it exists otherwise use etc/app.config
# if [ -e "$RUNNER_BASE_DIR/releases/$APP_VSN/sys.config" ]; then
#     CONFIG_PATH="$RUNNER_BASE_DIR/releases/$APP_VSN/sys.config"
# else
#     CONFIG_PATH="$RUNNER_ETC_DIR/app.config"
# fi
CONFIG_PATH="$RUNNER_ETC_DIR/app.config"


#
# Generate conf files - [app.config, vm.args]
#
gen_config() {
    rm -f $RUNNER_ETC_DIR/app.*.config
    rm -f $RUNNER_ETC_DIR/vm.*.args
    ERTS_PATH=$RUNNER_BASE_DIR/erts-$ERTS_VSN/bin
    NODETOOL_LITE="$ERTS_PATH/escript $ERTS_PATH/nodetool"
    # Check for presence of extra config files and process them in alphabetical order, if they exist
    ADDITIONAL_CONFIG_CMD=""
    if [ -d $RUNNER_ETC_DIR/$NODE_TYPE.d ] && ls -A $RUNNER_ETC_DIR/$NODE_TYPE.d/*.conf > /dev/null 2>&1
    then
        for config_name in $RUNNER_ETC_DIR/$NODE_TYPE.d/*.conf
        do
            ADDITIONAL_CONFIG_CMD="$ADDITIONAL_CONFIG_CMD -c $config_name"
        done
    fi
    RES_CUTTLEFISH=`PATH=$ERTS_PATH:$PATH $RUNNER_BASE_DIR/bin/cuttlefish -i $RUNNER_SCHEMA_DIR/$NODE_TYPE.schema -c $RUNNER_ETC_DIR/$NODE_TYPE.conf $ADDITIONAL_CONFIG_CMD -d $RUNNER_ETC_DIR/`

    APP_CONFIG=`find $RUNNER_ETC_DIR/ -type f | grep app.*[0-9].config`
    mv $APP_CONFIG $RUNNER_ETC_DIR/app.config

    VM_ARGS=`find $RUNNER_ETC_DIR/ -type f | grep vm.*[0-9].args`
    mv $VM_ARGS $RUNNER_ETC_DIR/vm.args

    # Sanity check the app.config file
    RES=`$NODETOOL_LITE chkconfig $RUNNER_ETC_DIR/app.config`
    if [ "$RES" != "ok" ]; then
        echo "Error reading $RUNNER_ETC_DIR/app.config"
        echo $RES
        exit 1
    fi
}
case "$1" in
    start|console|foreground_start)
        gen_config
        ;;
    *)
        ;;
esac


# Extract the target node name from node.args
NAME_ARG=`egrep '^-s?name' $VMARGS_PATH`
if [ -z "$NAME_ARG" ]; then
    echo "vm.args needs to have either -name or -sname parameter."
    exit 1
fi

# Extract the name type and name from the NAME_ARG for REMSH
REMSH_TYPE=`echo $NAME_ARG | awk '{print $1}'`
REMSH_NAME=`echo $NAME_ARG | awk '{print $2}'`

# Note the `date +%s`, used to allow multiple remsh to the same node transparently
REMSH_NAME_ARG="$REMSH_TYPE remsh`date +%s`@`echo $REMSH_NAME | awk -F@ '{print $2}'`"
REMSH_REMSH_ARG="-remsh $REMSH_NAME"

# Extract the target cookie
COOKIE_ARG=`grep '^-setcookie' $VMARGS_PATH`
if [ -z "$COOKIE_ARG" ]; then
    echo "vm.args needs to have a -setcookie parameter."
    exit 1
fi

# Add ERTS bin dir to our path
ERTS_PATH=$RUNNER_BASE_DIR/erts-$ERTS_VSN/bin

# Setup command to control the node
NODETOOL="$ERTS_PATH/escript $ERTS_PATH/nodetool $NAME_ARG $COOKIE_ARG"

# Setup remote shell command to control node
REMSH="$ERTS_PATH/erl $REMSH_NAME_ARG $REMSH_REMSH_ARG $COOKIE_ARG"

case "$1" in
    dump-mnesia-data|load-mnesia-data)
        if [ "$NODE_TYPE" != leo_manager ]; then
            echo "Only leo_manager supports {dump-mnesia-data|load-mnesia-data} commands"
            exit 1
        fi
    ;;  # actual implementation follows
esac


# Check the first argument for instructions
case "$1" in
    start)
        # Make sure there is not already a node running
        RES=`$NODETOOL ping`
        if [ "$RES" = "pong" ]; then
            echo "Node is already running!"
            exit 1
        fi

        HEART_COMMAND="$RUNNER_BASE_DIR/bin/$SCRIPT start"
        export HEART_COMMAND

        ERL_CRASH_DUMP_SECONDS=${ERL_CRASH_DUMP_SECONDS:-0}
        export ERL_CRASH_DUMP_SECONDS

        mkdir -p $PIPE_DIR
        shift # remove $1
        $ERTS_PATH/run_erl -daemon $PIPE_DIR $RUNNER_LOG_DIR "exec $RUNNER_BASE_DIR/bin/$SCRIPT console $@" 2>&1
        ;;

    stop)
        # Wait for the node to completely stop...
        case `uname -s` in
            Linux)
                # Only look for processes launched with the same NODE_EXTRA_NAME env variable
                # This will fail (won't find anything) when stopping nodes launched by older version of script
                # PID COMMAND+ENVIRONMENT
                PID=`ps axeww o pid,args|\
                    grep "NODE_EXTRA_NAME=$NODE_EXTRA_NAME " |\
                    grep "$RUNNER_BASE_DIR/.*/[b]eam"|awk '{print $1}'`
                ;;
            Darwin|FreeBSD|DragonFly|NetBSD|OpenBSD)
                # PID COMMAND
                PID=`ps ax -o pid= -o command=|\
                    grep "$RUNNER_BASE_DIR/.*/[b]eam"|awk '{print $1}'`
                ;;
            SunOS)
                # PID COMMAND
                PID=`ps -ef -o pid= -o args=|\
                    grep "$RUNNER_BASE_DIR/.*/[b]eam"|awk '{print $1}'`
                ;;
            CYGWIN*)
                # UID PID PPID TTY STIME COMMAND
                PID=`ps -efW|grep "$RUNNER_BASE_DIR/.*/[b]eam"|awk '{print $2}'`
                ;;
        esac
        $NODETOOL stop
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        while `kill -0 $PID 2>/dev/null`;
        do
            sleep 1
        done
        ;;

    restart)
        ## Restart the VM without exiting the process
        $NODETOOL restart
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;

    reboot)
        ## Restart the VM completely (uses heart to restart it)
        $NODETOOL reboot
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;

    ping)
        ## See if the VM is alive
        $NODETOOL ping
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;

    dump-mnesia-data)
        ## Dump mnesia-data to a textfile
        if [ $# -eq 2 ]; then
            $NODETOOL rpc mnesia dump_to_textfile "$2"
        else
            echo "Usage: leo_manager dump-mnesia-data <absolute-path>"
        fi

        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;
    load-mnesia-data)
        ## Load mnesia-data from the textfile
        if [ $# -eq 2 ]; then
            $NODETOOL rpc mnesia load_textfile "$2"
        else
            echo "Usage: leo_manager load-mnesia-data <absolute-path>"
        fi

        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;
    attach)
        # Make sure a node IS running
        RES=`$NODETOOL ping`
        ES=$?
        if [ "$ES" -ne 0 ]; then
            echo "Node is not running!"
            exit $ES
        fi

        shift
        exec $ERTS_PATH/to_erl $PIPE_DIR
        ;;

    reload_http_conf)
        $NODETOOL reload_http_conf
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;

    test_http_conf)
        $NODETOOL test_http_conf
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;

    ready)
        ## See if application is ready
        $NODETOOL ready
        ES=$?
        if [ "$ES" -ne 0 ]; then
            exit $ES
        fi
        ;;

    wait_ready)
        ## Wait until application is ready
        ES=1
        RETRY=0
        until [ $ES -eq 0 -o $RETRY -ge 30 ]
        do
            $NODETOOL ready
            ES=$?
            RETRY=$((RETRY + 1))
            sleep 1
        done
        if [ $ES -ne 0 ]; then
            echo "Not ready after $RETRY reties"
            exit 1
        fi
        ;;

    console|console_clean|foreground_start)
        if [ "$1" = "foreground_start" ]
        then
            # Start in foreground and without run_erl wrapper (for systemd, docker etc)
            # run_erl hides real exit code of erlexec, which prevents knowing if the process had crashed
            # or exited cleanly
            RES=`$NODETOOL ping`
            if [ "$RES" = "pong" ]; then
                echo "Node is already running!"
                exit 1
            fi

            ERL_CRASH_DUMP_SECONDS=${ERL_CRASH_DUMP_SECONDS:-0}
            export ERL_CRASH_DUMP_SECONDS
        fi

        # .boot file typically just $SCRIPT (ie, the app name)
        # however, for debugging, sometimes start_clean.boot is useful:
        case "$1" in
            console|foreground_start)  BOOTFILE=$SCRIPT ;;
            console_clean)             BOOTFILE=start_clean ;;
        esac
        # Setup beam-required vars
        ROOTDIR=$RUNNER_BASE_DIR
        BINDIR=$ROOTDIR/erts-$ERTS_VSN/bin
        EMU=beam
        PROGNAME=`echo $0 | sed 's/.*\\///'`
        case "$1" in
            console*)          CMD="$BINDIR/erlexec -heart -boot $RUNNER_BASE_DIR/releases/$APP_VSN/$BOOTFILE -mode $ERLEXEC_MODE -config $CONFIG_PATH -args_file $VMARGS_PATH -- ${1+"$@"}" ;;
            foreground_start)  CMD="$BINDIR/erlexec -noinput -boot $RUNNER_BASE_DIR/releases/$APP_VSN/$BOOTFILE -mode $ERLEXEC_MODE -config $CONFIG_PATH -args_file $VMARGS_PATH -- console" ;;
        esac
        export EMU
        export ROOTDIR
        export BINDIR
        export PROGNAME

        # Dump environment info for logging purposes
        echo "Config path: $RUNNER_ETC_DIR"
        echo "Exec: $CMD"
        echo "Root: $ROOTDIR"

        # Log the startup (only for "console" and "console_clean")
        [ "$1" != "foreground_start" ] && logger -t "$SCRIPT[$$]" "Starting up"

        # Start the VM
        exec $CMD
        ;;

    remote_console)
        # Make sure a node IS running
        RES=`$NODETOOL ping`
        ES=$?
        if [ "$ES" -ne 0 ]; then
            echo "Node is not running!"
            exit $ES
        fi

        shift
        exec $REMSH
        ;;

    *)
        help
        exit 1
        ;;
esac

exit 0
