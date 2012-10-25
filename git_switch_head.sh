#!/bin/sh

if echo $1 | grep -q -v "^\{.\+\}$"; then
  branch_or_tag=\"${1}\"
else
  branch_or_tag=$1
fi

if [ $# -lt 1 ]; then
    echo 'missing operand. specify a git refspec'
    exit 1
fi
find . -name "rebar.config" -print0 |xargs -0 grep leo-project|awk -F ":" '{print $1}'|uniq|xargs sed -i -s "s/{git,\([^,]\+leo-project[^,]\+\),\\s\+[\"{]\([^\"]\+\)[\"}]}}/{git,\1, $branch_or_tag}}/g"
