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

# Valgrind-specific Git configuration, if appropriate.
if git rev-parse --is-inside-work-tree > /dev/null 2>&1 ; then
    echo "running: git configuration"
    git config blame.ignoreRevsFile .git-blame-ignore-revs
else
    echo "skipping: git configuration"
fi
