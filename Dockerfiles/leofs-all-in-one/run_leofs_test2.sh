#!/bin/sh

set -xe

USER=root ./bootstrap.sh start cache-test
sleep 3
/leofs_test2/_build/default/bin/leofs_test -d /leofs/package/ -b test
