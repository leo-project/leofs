#!/bin/bash

# Settings
MOUNT_HOST=localhost
MOUNT_DIR=/mnt/foo
BUCKET=bbb
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

# Tests
{
    # try block
    eval $CMD_MOUNT &&
    eval $CMD_DF &&
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

