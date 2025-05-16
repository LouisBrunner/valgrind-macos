#!/bin/bash

set -e

if [ -z "${LTP_SRC_DIR:-}" ]; then
    echo "ERROR: LTP_SRC_DIR needs to be set.  Dying, bye bye ..."
    exit 1
fi

ORIG_PATH=$PATH
SCRIPT_SRC=$(dirname $0)
LOGDIR=${LOGDIR:-$LTP_SRC_DIR/ltp}
DIFFCMD="diff -u"
VALGRIND="${VALGRIND:-$LTP_SRC_DIR/../../../vg-in-place}"
# For parallel testing, consider IO intensive jobs, take nproc into account
PARALLEL_JOBS=${PARALLEL_JOBS:-$(nproc)}
# TESTS env var may be specified to restrict testing to selected test cases
# Configure the LTP testsuite behavior per
# https://lore.kernel.org/ltp/20250505195003.GB137650@pevik/T/#t
export LTP_COLORIZE_OUTPUT=0
export LTP_REPRODUCIBLE_OUTPUT=1

# Initialize LOGDIR for bunsen upload (https://sourceware.org/bunsen/)
mkdir -p $LOGDIR; rm -rf ${LOGDIR:?}/*


doTest ()
{
    t=$1
    nr=$2
    dir=$(dirname $t)
    exe=$(basename $t)
    l="$LOGDIR/$dir/$exe"
    rv="PASS"
    mkdir -p $l
    echo "[$nr/$c] Testing $exe ..."
    pushd $dir >/dev/null
        PATH="$ORIG_PATH:$PWD"
        t1=$(date +%s)
        ./$exe >$l/log1std 2>$l/log1err ||:
        $VALGRIND -q --tool=none --log-file=$l/log2 ./$exe >$l/log2std 2>$l/log2err ||:
        $VALGRIND -q --tool=memcheck --log-file=$l/log3 ./$exe >$l/log3std 2>$l/log3err ||:
        t2=$(date +%s)

        # We want to make sure that LTP syscall tests give identical
        # results with and without valgrind.  The test logs go to the
        # stderr.  They aren't identical across individual runs. The
        # differences include port numbers, temporary files, test
        # output ordering changes and more. They aren't trivially
        # comparable.  We resort to comparing at least the final
        # summary of individual test results
        tail -10 $l/log1err | grep -E "^(passed|failed|broken|skipped|warnings)" > $l/log1summary ||:
        tail -10 $l/log2err | grep -E "^(passed|failed|broken|skipped|warnings)" > $l/log2summary ||:
        tail -10 $l/log3err | grep -E "^(passed|failed|broken|skipped|warnings)" > $l/log3summary ||:

        # Check logs, report errors
        pushd $l >/dev/null
            if test -s log2; then
                echo -e "${exe}: unempty log2:\n$(cat log2)" | tee -a $l/$exe.log
                rv="FAIL"
            fi

            if grep -f $SCRIPT_SRC/ltp-error-patterns.txt log* > error-patterns-found.txt; then
                echo -e "${exe}: error string found:\n$(cat error-patterns-found.txt)" | tee -a $l/$exe.log
                rv="FAIL"
            fi

            if ! ${DIFFCMD} log1summary log2summary >/dev/null; then
                echo -e "${exe}: ${DIFFCMD} log1summary log2summary:\n$(${DIFFCMD} log1summary log2summary)" | tee -a $l/$exe.log
                rv="FAIL"
            fi

            if ! ${DIFFCMD} log2summary log3summary >/dev/null; then
                echo -e "${exe}: ${DIFFCMD} log2summary log3summary:\n$(${DIFFCMD} log2summary log3summary)" | tee -a $l/$exe.log
                rv="FAIL"
            fi

            # synthetize automake style testlogs for bunsen import
            echo ":test-result: $rv" | tee -a $l/$exe.log > $l/$exe.trs
            echo "Test time secs: $((t2 - t1))" > $l/test-suite.log
        popd >/dev/null
    popd >/dev/null
}

cd $LTP_SRC_DIR

if [ -n "$TESTS" ]; then
    echo "Running individual syscall tests specified in the TESTS env var ..."
    mapfile -t files < <(find testcases/kernel/syscalls -executable -and -type f \
                         | sort | grep -f <(echo $TESTS | tr ' ' '\n' | sed 's/.*/\/\0$/'))
else
    echo "Running whole the LTP syscall testsuite ..."
    mapfile -t files < <(find testcases/kernel/syscalls -executable -and -type f \
                         | sort | grep -v -f $SCRIPT_SRC/ltp-excludes.txt)
fi

c=${#files[@]}; i=0

# Run tests in parallel
for test in "${files[@]}"; do
    while test "$(jobs -l | wc -l)" -gt $PARALLEL_JOBS; do sleep 0.1; done
    i=$((++i))
    doTest $test $i &
done

wait

echo "TESTING FINISHED, logs in $LOGDIR"
