#!/bin/sh

#-------------------------------------------------------------------------------
# GLOBALS
#-------------------------------------------------------------------------------
NCLUSTERS=2
CLUSTER_NSTORAGES=4

#-------------------------------------------------------------------------------
# ROUTINES
#-------------------------------------------------------------------------------
usage()
{
    echo "usage: $0 [help|-h|--help]"                                        >&2
    echo "       $0 <command> [<args>]"                                      >&2
    echo ""                                                                  >&2
    echo "Commands:"                                                         >&2
    echo ""                                                                  >&2
    echo "  help                    Display usage"                           >&2
    echo "  clean                   Clean environment"                       >&2
    echo "  deploy                  Build and deploy environment"            >&2
    echo "  start  [<cl> [<node>]]  Start nodes"                             >&2
    echo "  status [<cl> [<node>]]  Show status"                             >&2
    echo "  stop   [<cl> [<node>]]  Stop nodes"                              >&2
    echo "  ping   [<cl> [<node>]]  Ping nodes"                              >&2
    echo "  adm    [<cl> [args]]    Run leofs-adm"                           >&2
    echo "  rmsh    <cl> <node>     Run remote shell on node"                >&2
    echo ""                                                                  >&2
    echo "Examples:"                                                         >&2
    echo ""                                                                  >&2
    echo "  ./mdcr.sh deploy"                                                >&2
    echo "  ./mdcr.sh start"                                                 >&2
    echo "  ./mdcr.sh ping"                                                  >&2
    echo "  ./mdcr.sh adm c1 start"                                          >&2
    echo "  ./mdcr.sh adm c2 start"                                          >&2
    echo "  ./mdcr.sh adm c1 status"                                         >&2
    echo "  ./mdcr.sh adm c1 join-cluster \\"                                >&2
    echo "      manager_10@127.0.0.1:13095 \\"                               >&2
    echo "      manager_11@127.0.0.1:13096"                                  >&2
    echo "  ./mdcr.sh adm c1 cluster-status"                                 >&2
    echo "  ./mdcr.sh ping c1 manager_0"                                     >&2
    echo "  ./mdcr.sh rmsh c1 manager_1"                                     >&2
    echo ""                                                                  >&2
    exit 1
}

#-------------------------------------------------------------------------------
validate_cluster_name()
{
    local name=$1 ; shift

    case "${name}" in
	c[0-9])
	    ;;
	*)
	    echo "invalid cluster name: ${name}" >&2
	    exit 1
	    ;;
    esac
}

#-------------------------------------------------------------------------------
cluster_name()
{
    local id=$1 ; shift

    echo "c$((id + 1))"
}

#-------------------------------------------------------------------------------
cluster_id()
{
    local name=$1 ; shift

    echo "$((${name#c} - 1))"
}

#-------------------------------------------------------------------------------
clean()
{
    echo "Cleaning..."

    rm -rf package/leo_*
    rm -rf package/c*/leo_*

    echo "Cleaned."
}

#-------------------------------------------------------------------------------
deploy()
{
    local i j cname

    clean

    echo "Deploying mdcr..."

    make release_for_test

    for i in `seq 0 $((NCLUSTERS - 1))`
    do
	cname=`cluster_name ${i}`

	mkdir -p package/${cname}

	cp -r package/leo_manager_* package/${cname}
	cp -r package/leo_gateway package/${cname}/leo_gateway_0
	for j in `seq 0 $((CLUSTER_NSTORAGES - 1))`
	do
	    cp -r package/leo_storage package/${cname}/leo_storage_${j}
	done

	cp priv/test/mdcr-test/${cname}/leo_manager.conf.0 \
	    package/${cname}/leo_manager_0/etc/leo_manager.conf
	cp priv/test/mdcr-test/${cname}/leo_manager.conf.1 \
	    package/${cname}/leo_manager_1/etc/leo_manager.conf
	cp priv/test/mdcr-test/${cname}/leo_gateway.conf \
	    package/${cname}/leo_gateway_0/etc/leo_gateway.conf
	for j in `seq 0 $((CLUSTER_NSTORAGES - 1))`
	do
	    cp priv/test/mdcr-test/${cname}/leo_storage_${j}.conf \
		package/${cname}/leo_storage_${j}/etc/leo_storage.conf
	done

	sed -i.orig -e 's/^\({intAgentUDPPort,\).*$/\1 4'${i}'20}./' \
	    package/${cname}/leo_manager_0/snmp/snmpa_manager_0/agent/conf/agent.conf
	sed -i.orig -e 's/^\({intAgentUDPPort,\).*$/\1 4'${i}'21}./' \
	    package/${cname}/leo_manager_1/snmp/snmpa_manager_1/agent/conf/agent.conf
	for j in `seq 0 $((CLUSTER_NSTORAGES - 1))`
	do
	    sed -i.orig -e 's/^\({intAgentUDPPort,\).*$/\1 4'${i}'1'${j}'}./' \
		package/${cname}/leo_storage_${j}/snmp/snmpa_storage_${j}/agent/conf/agent.conf
	done
	sed -i.orig -e 's/^\({intAgentUDPPort,\).*$/\1 4'${i}'00}./' \
	    package/${cname}/leo_gateway_0/snmp/snmpa_gateway_0/agent/conf/agent.conf
    done

    rm -rf package/leo_manager_*
    rm -rf package/leo_storage
    rm -rf package/leo_gateway

    echo "Deployed."
}

#-------------------------------------------------------------------------------
rc()
{
    local cmd clusters nodes
    # ugly hack for ubuntu
    if [ "$#" -ne "0" ]
    then
        cmd="$1"; shift
    fi
    if [ "$#" -ne "0" ]
    then
        clusters="$1"; shift
    fi
    if [ "$#" -ne "0" ]
    then
        nodes="$1"; shift
    fi
    local i c n rc status

    if [ -z "${clusters}" ]
    then
	for i in `seq 0 $((NCLUSTERS - 1))`
	do
	    clusters="${clusters} $(cluster_name ${i})"
	done
    else
	for c in ${clusters}
	do
	    validate_cluster_name "${c}"
	done
    fi

    status=0

    for c in ${clusters}
    do
	validate_cluster_name "${c}"

	if [ -z "${nodes}" ]
	then
	    nodes="manager_0 manager_1"
	    for i in `seq 0 $((CLUSTER_NSTORAGES - 1))`
	    do
		nodes="${nodes} storage_${i}"
	    done
	    nodes="${nodes} gateway_0"
	fi

	for n in ${nodes}
	do
	    rc="./package/${c}/leo_${n}/bin/leo_${n%_[0-9]}"

	    if [ ! -x "${rc}" ]
	    then
		echo "can't find rc script (${rc}) for cluster/node ${c}/${n}" >&2
		exit 1;
	    fi

	    echo -n "${cmd} ${c} ${n} "

	    "${rc}" ${cmd} || status=1

	    [ "${cmd}" = start ] && echo 'ok'
	done
    done

    return ${status}
}

#-------------------------------------------------------------------------------
adm()
{
    local cluster=$1 ; shift || :
    local id port

    if [ -z "$cluster" ]
    then
	cluster=c1
    fi

    validate_cluster_name "${cluster}"

    id=`cluster_id ${cluster}`

    port="10${id}10"

    ./leofs-adm -p "${port}" "$@"
}

#-------------------------------------------------------------------------------
rmsh()
{
    local cluster=$1 ; shift
    local node=$1    ; shift

    validate_cluster_name "${cluster}"

    rc remote_console "${cluster}" "${node}" "$@"
}

#-------------------------------------------------------------------------------
status()
{
    local clusters="$1" ; shift || :
    local nodes="$1"    ; shift || :
    local adm_clusters i c n rc status

    if [ -z "${clusters}" ]
    then
	for i in `seq 0 $((NCLUSTERS - 1))`
	do
	    clusters="${clusters} $(cluster_name ${i})"
	done
    else
	for c in ${clusters}
	do
	    validate_cluster_name "${c}"
	done
    fi

    status=0
    adm_clusters=

    for c in ${clusters}
    do
	validate_cluster_name "${c}"

	if [ -z "${nodes}" ]
	then
	    nodes="manager_0 manager_1"
	    for i in `seq 0 $((CLUSTER_NSTORAGES - 1))`
	    do
		nodes="${nodes} storage_${i}"
	    done
	    nodes="${nodes} gateway_0"
	fi

	for n in ${nodes}
	do
	    echo -n "status ${c} ${n} "

	    if rc ping ${c} "${n}" > /dev/null
	    then
		echo up

		if [ -z "${run_adm_status}" -a ${n} = manager_0 ]
		then
		    adm_clusters="${adm_clusters} ${c}"
		fi
	    else
		echo down
	    fi
	done
    done

    for c in ${adm_clusters}
    do
	adm ${c} status
	adm ${c} cluster-status
    done

    return ${status}
}

#-------------------------------------------------------------------------------
# MAIN
#-------------------------------------------------------------------------------
set -e

if [ -z "$1" ]
then
    usage
fi

cmd=$1 ; shift

case "${cmd}" in
    help|-h|--help)
	usage
	;;
    clean)
	clean
	;;
    deploy)
	deploy
	;;
    start|stop|ping)
	rc ${cmd} "$@"
	;;
    status)
	status "$@"
	;;
    adm)
	adm "$@"
	;;
    rmsh)
	[ $# -ge 2 ] || usage
	rmsh "$@"
	;;
    *)
	echo "Invalid command: ${cmd}" >&2
	usage
	;;
esac
