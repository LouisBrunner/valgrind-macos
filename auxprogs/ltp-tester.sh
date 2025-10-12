#!/bin/bash

set -e

if [ -z "${LTP_SRC_DIR:-}" ]; then
    echo "ERROR: LTP_SRC_DIR needs to be set.  Dying, bye bye ..."
    exit 1
fi

ORIG_PATH=$PATH
SCRIPT_SRC=$(dirname $0)
LOGDIR=${LOGDIR:-$LTP_SRC_DIR/ltp/tests}
DIFFCMD="diff -u"
# vgdb sensitive testcase: e.g. nftw01 nftw6401 setfsgid04 setfsgid03_16 symlink03
VGARGS="-q --vgdb=no"
VALGRIND="${VALGRIND:-$LTP_SRC_DIR/../../../vg-in-place} ${VGARGS}"
# For parallel testing, consider IO intensive jobs, take nproc into account
PARALLEL_JOBS=${PARALLEL_JOBS:-$(nproc)}
# TESTS env var may be specified to restrict testing to selected test cases
# Configure the LTP testsuite behavior per
# https://lore.kernel.org/ltp/20250505195003.GB137650@pevik/T/#t
export LTP_COLORIZE_OUTPUT=0
export LTP_REPRODUCIBLE_OUTPUT=1
export LTP_QUIET=1

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
        ./$exe >$l/log1std 2>$l/log1err ||:
        $VALGRIND --tool=none --log-file=$l/log2 ./$exe >$l/log2std 2>$l/log2err ||:
        $VALGRIND --tool=memcheck --log-file=$l/log3 ./$exe >$l/log3std 2>$l/log3err ||:

        for i in "$l"/log{1std,1err,2,2std,2err,3,3std,3err}; do
            echo "# cat $(basename $i)" >> $LOGDIR/$exe.log
            cat $i >> $LOGDIR/$exe.log
        done

        echo "# errors" >> $LOGDIR/$exe.log

        # If there is a logfile filter, apply it
        if test -f "${SCRIPT_SRC}/filters/${exe}"; then
            cat $l/log2 | ${SCRIPT_SRC}/filters/${exe} > $l/log2.filtered
        else
            cat $l/log2 > $l/log2.filtered
        fi

        # Check logs, report errors
        pushd $l >/dev/null
            if test -s log2.filtered; then
                echo -e "${exe}: unempty log2.filtered:\n$(cat log2.filtered)" | tee -a $LOGDIR/$exe.log
                rv="FAIL"
            fi

            if grep -f $SCRIPT_SRC/ltp-error-patterns.txt log* > error-patterns-found.txt; then
                echo -e "${exe}: error string found:\n$(cat error-patterns-found.txt)" | tee -a $LOGDIR/$exe.log
                rv="FAIL"
            fi

            if ! ${DIFFCMD} log1err log2err >/dev/null; then
                echo -e "${exe}: ${DIFFCMD} log1err log2err:\n$(${DIFFCMD} log1err log2err)" | tee -a $LOGDIR/$exe.log
                rv="FAIL"
            fi

            if ! ${DIFFCMD} log2err log3err >/dev/null; then
                echo -e "${exe}: ${DIFFCMD} log2err log3err:\n$(${DIFFCMD} log2err log3err)" | tee -a $LOGDIR/$exe.log
                rv="FAIL"
            fi

            # synthetize automake style testlogs for bunsen import
            echo "# result" >> $LOGDIR/$exe.log
            echo ":test-result: $rv" | tee -a $LOGDIR/$exe.log > $LOGDIR/$exe.trs
        popd >/dev/null
    popd >/dev/null
}

cd $LTP_SRC_DIR

echo "See *.log files for details on each test in this directory." > $LOGDIR/test-suite.log

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

echo -e "\nBrief LTP test results summary"
echo "-----------------------------------------"
find $LOGDIR -type f -name '*.trs' -exec grep -F ':test-result:' '{}' ';' |\
    sort -r | uniq -c | awk '{print $NF": "$1}'
echo -e "-----------------------------------------\n"

echo "TESTING FINISHED, logs in $LOGDIR"
