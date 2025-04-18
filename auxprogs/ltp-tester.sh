#!/bin/bash

set -e

if [ -z "${LTP_SRC_DIR:-}" ]; then
    echo "ERROR: LTP_SRC_DIR needs to be set.  Dying, bye bye ..."
    exit 1
fi

ORIG_PATH=$PATH
ORIG_PWD=$PWD
LOGDIR=${LOGDIR:-$LTP_SRC_DIR/valgrind-ltp-logs}
SUMMARY_LOG="$LOGDIR/summary.log"
DIFFCMD="diff -u"
VALGRIND="${VALGRIND:-$LTP_SRC_DIR/../../../vg-in-place}"

# Initialize LOGDIR
mkdir -p $LOGDIR; rm -rf $LOGDIR/*

myLog ()
{
    msg="$1"
    echo "$msg"
    echo -e "FAIL: $msg" >> $SUMMARY_LOG
}

cd $LTP_SRC_DIR

mapfile -t files < <(find testcases/kernel/syscalls -executable -and -type f \
                     | sort | grep -v -f $ORIG_PWD/ltp-excludes.txt)
c=${#files[@]}; i=0

for test in "${files[@]}"; do
    dir=$(dirname $test)
    exe=$(basename $test)
    l="$LOGDIR/$exe"
    mkdir -p $l
    i=$((++i))
    pushd $dir >/dev/null
        echo "[$i/$c] Testing $exe ..." | tee -a $SUMMARY_LOG
        PATH="$ORIG_PATH:$PWD"
        ./$exe >$l/log1std 2>$l/log1err ||:
        $VALGRIND -q --tool=none --log-file=$l/log2 ./$exe >$l/log2std 2>$l/log2err ||:
        $VALGRIND -q --tool=memcheck --log-file=$l/log3 ./$exe >$l/log3std 2>$l/log3err ||:

        # We want to make sure that LTP syscall tests give identical
        # results with and without valgrind.  The test logs go to the
        # stderr.  They aren't identical across individual runs. The
        # differences include port numbers, temporary files, test
        # output ordering changes and more. They aren't trivially
        # comparable.  We resort to comparing at least the final
        # summary of individual test results
        tail -10 $l/log1err | grep -E "^(passed|failed|broken|skipped|warnings):" > $l/log1summary ||:
        tail -10 $l/log2err | grep -E "^(passed|failed|broken|skipped|warnings):" > $l/log2summary ||:
        tail -10 $l/log3err | grep -E "^(passed|failed|broken|skipped|warnings):" > $l/log3summary ||:

        # Check logs, report errors
        pushd $l >/dev/null
            if test -s log2; then
                myLog "${exe}: unempty log2:\n$(cat log2)"
            fi

            if grep -f $ORIG_PWD/ltp-error-patterns.txt * > error-patterns-found.txt; then
                myLog "${exe}: error string found:\n$(cat error-patterns-found.txt)"
            fi

            if ! ${DIFFCMD} log1summary log2summary >/dev/null; then
                myLog "${exe}: ${DIFFCMD} log1summary log2summary:\n$(${DIFFCMD} log1summary log2summary)"
            fi

            if ! ${DIFFCMD} log2summary log3summary >/dev/null; then
                myLog "${exe}: ${DIFFCMD} log2summary log3summary:\n$(${DIFFCMD} log2summary log3summary)"
            fi
        popd >/dev/null
    popd >/dev/null
done

echo "TESTING FINISHED, logs in $LOGDIR"

