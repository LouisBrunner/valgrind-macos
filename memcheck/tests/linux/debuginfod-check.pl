#! /usr/bin/perl
use warnings;
use strict;
use Cwd;
use IPC::Open3;

our $dir = Cwd::realpath("./.debuginfod");
our $pid = 0;

# Kill the server and remove temporary files.
sub cleanup_and_exit($) {
    my $exit_code = $_[0];
    mysystem("rm -rf $dir");
    if ($pid != 0) {
        kill "INT", $pid;
    }
    exit $exit_code;
}

# Propagate Ctrl-C and exit if command results in an error.
sub mysystem($)
{
    my $exit_code = system($_[0]);
    ($exit_code == 2) and cleanup_and_exit(1); # Exit if SIGINT
    if ($exit_code != 0) {
        #warn "Error while executing: $_[0]";
        cleanup_and_exit(1);
    }
    return $exit_code;
}

# Check that debuginfod and debuginfod-find can be found
mysystem("debuginfod --help > /dev/null");
mysystem("debuginfod-find --help > /dev/null");

$SIG{'INT'} = sub { cleanup_and_exit(1) };

my $testname = "debuginfod-check";
my $tmp = "$dir/debuginfod_test.tmp";
my $dbg = "$dir/dbg";
mysystem("rm -rf $dir");
mysystem("mkdir -p $dbg");

# Compile the test executable, strip its debuginfo and store it so
# that valgrind cannot find it without debuginfod.
mysystem("gcc -O0 -g -o $testname $testname.c");
mysystem("objcopy --only-keep-debug $testname $testname.debug");
mysystem("objcopy --strip-unneeded $testname");
mysystem("objcopy --add-gnu-debuglink=$testname.debug $testname");
mysystem("mv $testname.debug $dbg");
mysystem("readelf -n $testname > $tmp 2>&1");

my $buildid = "";
open(TMP, '<', $tmp);
while (my $out = <TMP>) {
    if ($out =~ /Build ID: ([0-9a-f]*)/) {
        $buildid = $1;
    }
}

if ($buildid eq "") {
    warn "can't find $testname build-id";
    cleanup_and_exit(1);
}

my $found_port = 0;
my $port = 7999;

# Find an unused port
while ($found_port == 0 and $port < 65536) {
    $port++;
    $pid = open3(undef, "TMP", undef,
                 "debuginfod", "-d", "$dir/db", '-F', "$dbg", "--port=$port");
    for (my $i = 0; $i < 5 and $found_port == 0; $i++) {
        while (my $got = <TMP>) {
            if ($got =~ /Failed to bind/) {
                last;
            } elsif ($got =~ /started http server/) {
                $found_port = 1;
                last;
            }
        }
    }
}

if ($port == 65536) {
    warn "No available ports";
    cleanup_and_exit(1);
}

my $server_ready = 0;

# Confirm that the server is ready to be queried
for (my $i = 0; $i < 10 and $server_ready == 0; $i++) {
    sleep 1;
    my $got = `curl -s http://localhost:$port/metrics`;
    if ($got =~ /ready 1/
        and $got =~ /thread_work_total\{role=\"traverse\"\} 1/
        and $got =~ /thread_work_pending\{role=\"scan\"\} 0/
        and $got =~ /thread_busy\{role=\"scan\"\} 0/) {
        $server_ready = 1;
    }
}

if ($server_ready == 0) {
    warn "Can't start debuginfod server";
    cleanup_and_exit(1);
}

# Query the server and store the debuginfo in the client cache for valgrind to find.
my $myres = mysystem("DEBUGINFOD_CACHE_PATH=$dir DEBUGINFOD_URLS=http://localhost:$port debuginfod-find debuginfo $buildid > /dev/null 2>&1");
if ($myres != 0) {
    cleanup_and_exit(1);
}
kill "INT", $pid;
exit 0;
