#!/bin/sh

# This script is used to do install_name_tool on the libmySystem.so library.
# The reason it is not done directly through gcc/ld is that due to the name
# of the library, it would make a "circular" dylib dependency (as we also link with -lSystem)
# and thus the argument is dropped by ld.
#
# The solution is to build the library normally and change the name afterwards.
#
# Therefore it is only used on macOS (Darwin), specifically for arm64 support.

if [ $# -lt 1 ]; then
    echo "Usage: $0 <link> [args...]"
    exit 1
fi

echo "disb: $@"
$@
if [ $? -ne 0 ]; then
    exit $?
fi
cmd="install_name_tool -id /usr/lib/libSystem.B.dylib libmySystem.so"
echo "disb: $cmd"
$cmd
exit $?
