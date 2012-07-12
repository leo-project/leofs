#!/bin/sh

if [ $# -lt 1 ]; then
    echo 'missing operand. specify a new version like 0.9.0'
    exit 1
fi
for repo in `cat git_repos.conf`
do
find $repo -regex ".+\.[he]rl" |xargs sed -i -s "s/-vsn('\([^']\+\)/-vsn('$1/g"
find $repo -name "*.app.src"   |xargs sed -i -s "s/{vsn,\\s\+\"\([^\"]\+\)/{vsn, \"$1/g"
done
