#!/bin/sh

if [ $# -lt 1 ]; then
    echo 'missing operand. specify a git refspec'
    exit 1
fi
find . -name "rebar.config" -print0 |xargs -0 grep leo-project|awk -F ":" '{print $1}'|uniq|xargs sed -i -s "s/{git,\([^,]\+leo-project[^,]\+\),\\s\+\"\([^\"]\+\)/{git,\1, \"$1/g"
