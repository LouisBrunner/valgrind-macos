#!/bin/sh

# We need a python3 binary
type python3 2>/dev/null 1>/dev/null || exit 1

# And it needs to support at least version 3.9
python3 -c 'import sys; assert sys.version_info >= (3,9)' 2>/dev/null || exit 1

exit 0
