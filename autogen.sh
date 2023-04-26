#!/bin/sh

run ()
{
    echo "running: $*"
    eval $*

    if test $? != 0 ; then
	echo "error: while running '$*'"
	exit 1
    fi
}

run aclocal
run autoheader
run automake -a
run autoconf

# Valgrind-specific Git configuration.
echo "running: git configuration"
git config blame.ignoreRevsFile .git-blame-ignore-revs
