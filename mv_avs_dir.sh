#!/bin/sh
## ======================================================================
##
## LeoFS
##
## Copyright (c) 2012-2017 Rakuten, Inc.
##
## This file is provided to you under the Apache License,
## Version 2.0 (the "License"); you may not use this file
## except in compliance with the License.  You may obtain
## a copy of the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing,
## software distributed under the License is distributed on an
## "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
## KIND, either express or implied.  See the License for the
## specific language governing permissions and limitations
## under the License.
##
## ======================================================================
## 'mv_avs_dir' which can be used to change the data directory.
##
## You just specify the current directory and the destination directory,
## and you need to modify the leo_storage configuration, 'obj_containers.path '
## before re-launching the LeoStorage node.
##
## Note: You need to stop a target LeoStorage node before executing 'mv_avs_dir'.
## ======================================================================

bold=`tput bold`
normal=`tput sgr0`
NONE='\033[00m'
BOLD='\033[1m'
UNDERLINE='\033[4m'
SCRIPT=`basename $0`

DIR_LOG="log"
DIR_OBJ="object"
DIR_METADATA="metadata"


## ------------------------------------
## Common Functions
## ------------------------------------
output() {
    echo "$1"
}

usage() {
    output "usage: ${bold}$SCRIPT${normal} <source-directory> <destination-directory>"
}


## Operates files
##   - Removes current similinks
##   - Creates new simlinks
## @param destination-directory
## @param delimiter
##
op_files() {
    dir=$1
    cd "$dir"

    for i in *
    do
        f=$i
        if  [ -L "$f" ] ; then
            l=`readlink "$f"`
            l=${l%/}
            l=${l##*/}
            rm "$f"

            source_file="$l"
            target_file="$f"
            ln -fs "$source_file" "$target_file"
            echo "[CREATED] $dir/$source_file > $dir/$target_file"
        fi
    done
    cd -
}


## ------------------------------------
## Validation
## ------------------------------------
if [ $# -ne 2 ]; then
    usage
    exit 1
fi

cur=$(pwd)
src=${1%/}
dest=${2%/}

if [ -e $src ]; then
    if [ -d $src ]; then
        if [ -e $dest ]; then
            if [ -d $dest ]; then
                echo "::: LeoFS / move-avs-directories :::"
                echo ""
                echo "[INFO]"
                echo "   Src dir: $src"
                echo "  Dest dir: $dest"
                echo ""
            else
                echo "[ERROR] $dest is NOT directory"
                exit 1
            fi
        else
            echo "[ERROR] $dest NOT Found"
            exit 1
        fi
    else
        echo "[ERROR] $src is NOT directory"
        exit 1
    fi
else
    echo "[ERROR] $src NOT Found"
    exit 1
fi


## ------------------------------------
## Processing For Destination Dir
## ------------------------------------
## @TODO
## mv $src/* $dest/
cp -pR $src/* $dest/

##
## Operates files
##
op_files "$dest/$DIR_LOG" "."
op_files "$dest/$DIR_OBJ" "_"
op_files "$dest/$DIR_METADATA" "_"
