#!/bin/bash

#####################
#       Setup       #
#####################
# In Storage Nodes
# 1. mkdir /ssd
# 2. tar nfs_dummy_test_avs.tar.gz -C /ssd
# 3. set obj_containers.path in leo_storage.conf to [/ssd/avs]

# Settings
MOUNT_HOST="${MOUNT_HOST:-localhost}"
MOUNT_DIR="${MOUNT_DIR:-/mnt/leofs}"
BUCKET="${BUCKET:-test/05236/bb5034f0c740148a346ed663ca0cf5157efb439f}"
RET=
FNAME_1K=1k.dat
FNAME_1M=1m.dat
FNAME_50M=50m.dat
FNAME_TOUCH=newfile

# Generate test files
TMP_UID=`uuidgen`
TMP_DIR=/tmp/$TMP_UID
mkdir -p $TMP_DIR
TMP_1K_FILE=$TMP_DIR/$FNAME_1K
DST_1K_FILE=$MOUNT_DIR/$FNAME_1K
TMP_1M_FILE=$TMP_DIR/$FNAME_1M
DST_1M_FILE=$MOUNT_DIR/$FNAME_1M
TMP_50M_FILE=$TMP_DIR/$FNAME_50M
DST_50M_FILE=$MOUNT_DIR/$FNAME_50M
DST_SUB_DIR=$MOUNT_DIR/sub1/sub2/sub3
DST_SUB_ROOT_DIR=$MOUNT_DIR/sub1
DST_50M_FILE2=$DST_SUB_DIR/$FNAME_50M
DST_50M_FILE3=$MOUNT_DIR/${FNAME_50M}.3
DST_50M_FILE4=$MOUNT_DIR/${FNAME_50M}.4
DST_OLD_DIR=$MOUNT_DIR/test_old_dir
DST_NEW_DIR=$MOUNT_DIR/test_new_dir
DST_BOTH_DIR=$MOUNT_DIR/test_both_dir
TOUCHED_FILE=$MOUNT_DIR/$FNAME_TOUCH
dd if=/dev/urandom of=$TMP_1K_FILE bs=1024 count=1
dd if=/dev/urandom of=$TMP_1M_FILE bs=1024 count=1024
dd if=/dev/urandom of=$TMP_50M_FILE bs=1048576 count=50

# Command templates
CMD_MOUNT="sudo mount -t nfs -o nolock $MOUNT_HOST:/$BUCKET $MOUNT_DIR"
CMD_UNMOUNT="sudo umount -f $MOUNT_DIR"
CMD_DF="df -h"
CMD_MKDIR="mkdir -p $DST_SUB_DIR"
CMD_CP_1K="cp $TMP_1K_FILE $DST_1K_FILE"
CMD_CP_1M="cp $TMP_1M_FILE $DST_1M_FILE"
CMD_CP_50M="cp $TMP_50M_FILE $DST_50M_FILE"
CMD_CP_50M3="cp $TMP_50M_FILE $DST_50M_FILE3"
CMD_CP_50M4="cp $TMP_50M_FILE $DST_50M_FILE4"
CMD_DIFF_1K="diff $TMP_1K_FILE $DST_1K_FILE"
CMD_DIFF_1M="diff $TMP_1M_FILE $DST_1M_FILE"
CMD_DIFF_50M="diff $TMP_50M_FILE $DST_50M_FILE"
CMD_RM_1K="rm $DST_1K_FILE"
CMD_MV_50M="mv $DST_50M_FILE $DST_50M_FILE2"
CMD_RM_50M="rm $DST_50M_FILE2"
CMD_RMDIR_SUB="rmdir $DST_SUB_DIR"
CMD_RM_SUB_ROOT="rm -rf $DST_SUB_ROOT_DIR"
CMD_TOUCH="touch $TOUCHED_FILE"
CMD_DD="dd if=$DST_50M_FILE3 of=$DST_50M_FILE4 bs=128 count=1"

# Functions
function ls_validate_num_of_childs() {
    DIR=$1
    TOBE=$2
    ASIS=`ls $DIR| wc -l`
    if [ $ASIS -eq $TOBE ]; then
        return 0
    else
        echo "[Failed]ls_validate_num_of_childs expected:$TOBE value:$ASIS"
        return 1
    fi
}

function find_validate_name_exp() {
    DIR=$1
    NAME=$2
    TOBE=$3
    if [ `find $DIR -name $NAME` = $TOBE ]; then
        return 0
    else
        echo "[Failed]find_validate_name_exp expected:$TOBE value:$NAME"
        return 1
    fi
}

function stat_validate_size() {
    FILE=$1
    SIZE=$2
    TOBE=`stat $FILE --format=%s`
    if [ $TOBE -eq $SIZE ]; then
        return 0
    else
        echo "[Failed]stat_validate_size expected:$TOBE value:$SIZE"
        return 1
    fi
}

function du_validate_size_in_mbyte() {
    DIR=$1
    SIZE=$2
    TOBE=`du -m $DIR|awk '{print $1}'`
    if [ $TOBE -eq $SIZE ]; then
        return 0
    else
        echo "[Failed]du_validate_size_in_mbyte expected:$TOBE value:$SIZE"
        return 1
    fi
}

function make_many_files() {
    DIR=$1
    NUM=$2
    for i in `seq 1 $NUM`
    do
        touch $DIR/$i
    done
    return 0
}

function check_dummy() {
    if [ ! -d $DST_OLD_DIR ]; then
        echo "[Failed] Old Dummy Dir File Missing"
        return 1
    elif [ ! -d $DST_NEW_DIR ]; then
        echo "[Failed] NEW Dummy Dir File Missing"
        return 1
    elif [ ! -d $DST_BOTH_DIR ]; then
        echo "[Failed] OLD+NEW Dummy Dir File Missing"
        return 1
    fi
    rmdir $DST_OLD_DIR
    rmdir $DST_NEW_DIR
    rmdir $DST_BOTH_DIR
    return 0
}

function drop_cache() {
    echo 3 | sudo tee /proc/sys/vm/drop_caches
}

function partial_write_test() {
    case $OSTYPE in
    linux*)
        TMP_FILE=$1
        TAR_FILE=$2
        OFFSET=$3
        dd if=$TMP_1M_FILE of=$TMP_FILE bs=$OFFSET conv=notrunc seek=1
        dd if=$TMP_1M_FILE of=$TAR_FILE bs=$OFFSET conv=notrunc seek=1
        drop_cache
        diff $TMP_FILE $TAR_FILE
        ;;
    *)
        return 0
        ;;
        esac
}

function partial_read_test() {
    case $OSTYPE in
    linux*)
        TMP_FILE=$1
        TAR_FILE=$2
        OFFSET=$3
        SIZE=$4
        drop_cache
        dd if=$TMP_FILE of=$TMP_DIR/part_read_ori skip=$OFFSET bs=$SIZE count=1 iflag=skip_bytes
        dd if=$TAR_FILE of=$TMP_DIR/part_read_tar skip=$OFFSET bs=$SIZE count=1 iflag=skip_bytes
        diff $TMP_DIR/part_read_ori $TMP_DIR/part_read_tar
        ;;
    *)
        return 0
        ;;
        esac
}

# Tests
{
    # try block
    eval $CMD_MOUNT &&
    eval $CMD_DF &&
    check_dummy &&
    eval $CMD_MKDIR &&
    eval $CMD_CP_1K &&
    eval $CMD_CP_1M &&
    eval $CMD_CP_50M &&
    eval $CMD_DIFF_1K &&
    eval $CMD_DIFF_1M &&
    eval $CMD_DIFF_50M &&
    ls_validate_num_of_childs $MOUNT_DIR 4 &&
    eval $CMD_RM_1K &&
    ls_validate_num_of_childs $MOUNT_DIR 3 &&
    eval $CMD_MV_50M &&
    ls_validate_num_of_childs $MOUNT_DIR 2 &&
    find_validate_name_exp $MOUNT_DIR $FNAME_50M $DST_50M_FILE2 &&
    eval $CMD_RM_50M &&
    eval $CMD_RMDIR_SUB &&
    eval $CMD_RM_SUB_ROOT &&
    ls_validate_num_of_childs $MOUNT_DIR 1 &&
    eval $CMD_TOUCH &&
    stat_validate_size $DST_1M_FILE 1048576 &&
    du_validate_size_in_mbyte $MOUNT_DIR 2 &&
    eval $CMD_CP_50M3 &&
    eval $CMD_CP_50M4 &&
    make_many_files $MOUNT_DIR 500 &&
    ls_validate_num_of_childs $MOUNT_DIR 504 &&
    eval $CMD_DD &&
    stat_validate_size $DST_50M_FILE4 128 &&
    partial_write_test $TMP_50M_FILE $DST_50M_FILE3 524288 &&
    partial_read_test $TMP_50M_FILE $DST_50M_FILE3 1572864 1048576 &&
    echo "[Success]All tests passed."
} || 
{
    # catch block
    RET=$?
    echo "[Error]Test failed. status=$RET"
}

# final block
eval $CMD_UNMOUNT
rm -rf $TMP_DIR
exit $RET

