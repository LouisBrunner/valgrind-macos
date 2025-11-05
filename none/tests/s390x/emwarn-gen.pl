#!/usr/bin/env perl

#--------------------------------------------------------------------
# Create testcases for checking BFP/DFP emulation warnings
#
# This machinery is to circumwent valgrind's limitation of at most
# 3 emulation warnings per emulation kind. 
#--------------------------------------------------------------------

use strict;
use warnings;
use Cwd 'abs_path';
use Getopt::Long;

my $rootdir  = get_rootdir();
my $runone   = "$rootdir/auxprogs/s390-runone";
my $valgrind = "$rootdir/vg-in-place";
my $valargs  = "-q --tool=none --show-emwarns=yes";

&main;

sub main
{
    GetOptions("bfp-XxC" => sub { test_bfp_XxC(); },
               "dfp-XiC" => sub { test_dfp_XiC(); },
               "dfp-XxC" => sub { test_dfp_XxC(); },
              ) or exit 1;
    exit 0;
}

sub test_bfp_XxC
{
    header("BFP CONVERT FROM FIXED");
    run_insn_test("cefbra 0,0,0,4");
    run_insn_test("cdfbra 0,0,0,4");
    run_insn_test("cxfbra 0,0,0,4");
    run_insn_test("cegbra 0,0,0,4");
    run_insn_test("cdgbra 0,0,0,4");
    run_insn_test("cxgbra 0,0,0,4");

    header("BFP CONVERT FROM LOGICAL");
    run_insn_test("celfbr 0,0,0,4");
    run_insn_test("cdlfbr 0,0,0,4");
    run_insn_test("cxlfbr 0,0,0,4");
    run_insn_test("celgbr 0,0,0,4");
    run_insn_test("cdlgbr 0,0,0,4");
    run_insn_test("cxlgbr 0,0,0,4");

    header("BFP CONVERT TO FIXED");
    run_insn_test("cfebra 0,0,0,4");
    run_insn_test("cfdbra 0,0,0,4");
    run_insn_test("cfxbra 0,0,0,4");
    run_insn_test("cgebra 0,0,0,4");
    run_insn_test("cgdbra 0,0,0,4");
    run_insn_test("cgxbra 0,0,0,4");

    header("BFP CONVERT TO LOGICAL");
    run_insn_test("clfebr 0,0,0,4");
    run_insn_test("clfdbr 0,0,0,4");
    run_insn_test("clfxbr 0,0,0,4");
    run_insn_test("clgebr 0,0,0,4");
    run_insn_test("clgdbr 0,0,0,4");
    run_insn_test("clgxbr 0,0,0,4");

    header("BFP LOAD FP INTEGER");
    run_insn_test("fiebra 0,0,0,4");
    run_insn_test("fidbra 0,0,0,4");
    run_insn_test("fixbra 0,0,0,4");

    header("BFP LOAD ROUNDED");
    run_insn_test("ledbra 0,0,0,4");
    run_insn_test("ldxbra 0,0,0,4");
    run_insn_test("lexbra 0,0,0,4");
}

sub test_dfp_XxC
{
    header("DFP CONVERT FROM FIXED");
    run_insn_test("cdgtra 0,0,0,4");
    run_insn_test("cxgtra 0,0,0,4");
    run_insn_test("cdftr  0,0,0,4");
    run_insn_test("cxftr  0,0,0,4");

    header("DFP CONVERT FROM LOGICAL");
    run_insn_test("cdlgtr 0,0,0,4");
    run_insn_test("cxlgtr 0,0,0,4");
    run_insn_test("cdlftr 0,0,0,4");
    run_insn_test("cxlftr 0,0,0,4");
    
    header("DFP CONVERT TO FIXED");
    run_insn_test("cgdtra 0,0,0,4");
    run_insn_test("cgxtra 0,0,0,4");
    run_insn_test("cfdtr  0,0,0,4");
    run_insn_test("cfxtr  0,0,0,4");

    header("DFP CONVERT TO LOGICAL");
    run_insn_test("clgdtr 0,0,0,4");
    run_insn_test("clgxtr 0,0,0,4");
    run_insn_test("clfdtr 0,0,0,4");
    run_insn_test("clfxtr 0,0,0,4");

    header("DFP LOAD ROUNDED");
    run_insn_test("ledtr 0,0,0,4");
    run_insn_test("ldxtr 0,0,0,4");
}

sub test_dfp_XiC
{
    header("DFP LOAD LENGTHENED");
    run_insn_test("ldetr 0,0,8");
    run_insn_test("lxdtr 0,0,8");
    
    header("DFP LOAD ROUNDED");
    run_insn_test("ledtr 0,0,0,8");
    run_insn_test("ldxtr 0,0,0,8");
}

sub run_insn_test
{
    my ($insn) = @_;
    $insn =~ s/\s+/ /g;
    my ($mnm) = $insn;

    $mnm =~ s/\s.*//;
    print "Testing:  $insn\n";

    my $exe = "xxemwarn-$mnm";
    my $cfile = "$exe.c";

    # Create template
    `$runone --template --insn=\"$insn\" > $cfile`;

    # Compile 
    my $stderr = `$runone --build $cfile 2>&1`;
    if ($? != 0) {
        error("runone failed\n$stderr");
        return;
    }

    # Run valgrind
    my $output = `$valgrind $valargs ./$exe 2>&1 | ./filter_stderr`;
    print STDOUT "$output\n";

    # Remove files 
    unlink ($exe, $cfile, "$exe.s", "$exe.s.orig");
}

sub get_rootdir
{
    my $dir = ".";
    while (abs_path($dir) ne "/") {
        if (-e "$dir/AUTHORS") {
            return abs_path($dir);
        }
        $dir = "$dir/..";
    }
    fatal("Coud not determine root directory. \"AUTHORS\" file not found\n");
}

sub header
{
    my ($txt) = @_;

    print "\n";
    print "============================================================\n";
    print "$txt\n";
    print "============================================================\n";
}

sub error
{
    print STDERR "*** $_[0]\n";
}

sub fatal
{
    error($_[0]);
    exit 1;
}
