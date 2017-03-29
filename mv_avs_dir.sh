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
## You just specify the current directory and the destination directory,
## and you need to modify the leo_storage configuration, 'obj_containers.path '.
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
QSTR_LOG="leo_object_storage_*"
QSTR_OBJ="*.avs"
QSTR_METADATA="*"


## ------------------------------------
## Common Functions
## ------------------------------------
output() {
    echo "$1"
}

usage() {
    output "usage: ${bold}$SCRIPT${normal} <source-directory> <destination-directory>"
}

rm_if_link(){
    [ ! -L "$1" ] || rm "$1";
}

rm_simlink() {
    cd $1
    for i in *
    do
        rm_if_link "$i"
    done
    cd -
}

create_simlink() {
    dir=$1
    cd "$dir"
    IFS=$2

    for i in *
    do
        org=$i
        set -- $i
        source_file="$dir/$org"
        target_file="$dir/$1"
        ln -s "$source_file" "$target_file"
        echo "[CREATED] $source_file > $target_file"
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
src=$1
dest=$2

if `echo $src | tail -c2 | grep -q '/'` ; then
    src=`echo $src | rev | cut -c 2- | rev`
fi
if `echo $dest | tail -c2 | grep -q '/'` ; then
    dest=`echo $dest | rev | cut -c 2- | rev`
fi


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
## Processing For Source Dir
## ------------------------------------
## leo_storage/avs/log
rm_simlink "$src/$DIR_LOG"

## leo_storage/avs/object
rm_simlink "$src/$DIR_OBJ"

## leo_storage/avs/metadata
rm_simlink "$src/$DIR_METADATA"


## ------------------------------------
## Processing For Destination Dir
## ------------------------------------
## mv $src/* $dest/
cp -r $src/* $dest/

## leo_storage/avs/log
create_simlink "$dest/$DIR_LOG" "."

## leo_storage/avs/object
create_simlink "$dest/$DIR_OBJ" "_"

## leo_storage/avs/metadata
create_simlink "$dest/$DIR_METADATA" "_"

echo "[FINISHED]"
