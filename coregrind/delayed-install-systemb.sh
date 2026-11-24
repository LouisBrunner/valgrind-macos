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
	echo "Usage: $0 <target> <link> [args...]"
	exit 1
fi
target="$1"
shift

echo "disb: $*"
"$@" -o "$target"
result=$?
if [ ${result} -ne 0 ]; then
	exit ${result}
fi
cmd="install_name_tool -id /usr/lib/libSystem.B.dylib ${target}"
echo "disb: $cmd"
exec $cmd
