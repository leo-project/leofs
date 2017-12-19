#!/bin/sh

# requires first parameter to be either "yes" or "no", which is done by "make reltool"
with_sd_notify=$1

sed "
1 i\%% CAUTION: Don't edit this file as it's automatically generated through \"make release\"
1 i\%% Edit \"reltool.config.in\" instead\n
b $with_sd_notify
:yes
s/%% SD_NOTIFY_PLACEHOLDER/,{app, sd_notify,             [{incl_cond, include}]}/
b
:no
/%% SD_NOTIFY_PLACEHOLDER/d" $2
