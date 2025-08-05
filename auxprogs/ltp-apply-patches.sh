#!/bin/bash

set -xe

LTP_SRC_DIR=$1
cd $(dirname $(readlink -f $0))
echo "applying LTP patches ..."
if compgen -G "ltp-patches/*.patch"; then
    for i in ltp-patches/*.patch; do
        patch -d "$LTP_SRC_DIR"  -p1 < "$i"
    done
    echo "applying LTP patches finished"
else
    echo "no patches found, nothing to do"
fi
