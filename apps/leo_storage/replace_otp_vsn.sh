#!/bin/sh
find . -name rebar.config|xargs sed -i 's/require_otp_vsn,\s\+"\(.\+\)"/require_otp_vsn, "R16B*|17|18|19"/g'
