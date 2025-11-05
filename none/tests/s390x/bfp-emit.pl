#!/usr/bin/env perl

#--------------------------------------------------------------------
# For a subset of the available BFP insns the following is true:
#
#   For a BFP insn X in the guest code the very same insn will be
#   emitted in the jitted code.
#
# This is because IR optimisers do not touch floating point IROps.
# Meaning: if we can show that for insn X the same insn X is emitted
# we do not need to proof read results of floating point computations.
# Exceptions are: checking order of operands for non-commutative
# operators and condition code computation. This is done elsewhere.
#
# Here we do the following:
# Craft a tiny program using insns X. Run valgrind on it, trace IR
# generation and assembly. Check the output making sure that insn X
# appears once in the "Frontend" section and once in the "Assembly"
# section.
#
# Below is a complete list of all BFP insns as of SA22-7832-14.
#--------------------------------------------------------------------

use strict;
use warnings;
use Cwd 'abs_path';

my $rootdir  = get_rootdir();
my $runone   = "$rootdir/auxprogs/s390-runone";
my $valgrind = "$rootdir/vg-in-place";
my $valargs  = "-q --tool=none --trace-notbelow=0 --trace-flags=10000001";


# Instructions that are always mapped independent of any rounding mode
# or whatever.
my %insn_map = (
    # l[cnp]dfr cannot be disinguished from l[cnp]dbr because they
    # use the same IROp.
    "lcdfr"  => "lcdbr",
    "lndfr"  => "lndbr",
    "lpdfr"  => "lpdbr",

    # The "and signal" part is currently ignored
    "kebr"   => "cebr",
    "kdbr"   => "cdbr",
    "kxbr"   => "cxbr",
    
    # c[fg][edx]br and c[fg][edx]bra differ only in the presence of an m4
    # field. That field cannot be represented in VEX IR and is therefore
    # ignored and assumed to be zero.
    "cfebra" => "cfebr",
    "cfdbra" => "cfdbr",
    "cfxbra" => "cfxbr",
    "cgebra" => "cgebr",
    "cgdbra" => "cgdbr",
    "cgxbra" => "cgxbr",

    # cdfbra 32-bit int -->  64-bit BFP  Iop_I32StoF64  has no rounding
    # cxfbra 32-bit int --> 128-bit BFP  Iop_I32StoF128 has no rounding mode
    # cxgbra 64-bit int --> 128-bit BFP  Iop_I64StoF128 has no rounding mode
    "cdfbra" => "cdfbr",
    "cxfbra" => "cxfbr",
    "cxgbra" => "cxgbr",

    # fi[edx]br and fi[edx]bra differ only in the presence of an m4 field.
    # That field cannot be represented in VEX IR and is therefore ignored
    # and assumed to be zero.
    "fiebra" => "fiebr",
    "fidbra" => "fidbr",
    "fixbra" => "fixbr",
);

&main;

sub main
{
    #-----------------------------------------------------------------------
    # POP Chapter 9: Floating-Point Overview and Support Instructions
    #-----------------------------------------------------------------------

    # CONVERT BFP TO HFP     not implemented
    # CONVERT HFP TO BFP     not implemented
    # COPY SIGN              insn not mapped one-to-one
    # EXTRACT FPC            just guest state access
    # LOAD                   just guest state access

    header("LOAD COMPLEMENT");
    test_insn("lcdfr %f0,%f1", \&mapper);

    # LOAD FPC               just guest state access
    # LOAD FPC AND SIGNAL    not implemented

    header("LOAD FPR FROM GR");
    test_insn("ldgr %f0,%r1");

    header("LOAD GR FROM FPR");
    test_insn("lgdr %r0,%f1");

    header("LOAD NEGATIVE");
    test_insn("lndfr %f0,%f1", \&mapper);

    header("LOAD POSITIVE");
    test_insn("lpdfr %f0,%f1", \&mapper);

    header("LOAD ZERO");
    test_insn("lzer %f0");
    test_insn("lzdr %f0");
    # test_insn("lzxr %f0")  insn not mapped one-to-one

    # PFPO                   insn not mapped one-to-one
    # SET BFP ROUNDING MODE  insn not mapped one-to-one
    # SET DFP ROUNDING MODE  insn not mapped one-to-one
    # SET FPC                just guest state access
    # SET FPC AND SIGNAL     not implemented
    # STORE                  insn not mapped one-to-one
    # STORE FPC              just store + guest state access

    #----------------------------------------------------------------------
    # POP Chapter 19: Binary-Floating-Point Instructions
    #----------------------------------------------------------------------

    header("ADD");
    test_insn("aebr %f0,%f1");
    test_insn("adbr %f0,%f1");
    test_insn("axbr %f0,%f1");

    header("COMPARE");
    test_insn("cebr %f0,%f1");
    test_insn("cdbr %f0,%f1");
    test_insn("cxbr %f0,%f1");

    header("COMPARE AND SIGNAL");
    test_insn("kebr %f0,%f1", \&mapper);
    test_insn("kdbr %f0,%f1", \&mapper);
    test_insn("kxbr %f0,%f1", \&mapper);

    header("CONVERT FROM FIXED");
    test_insn("cefbr %f0,%r1");
    test_insn("cdfbr %f0,%r1");
    test_insn("cxfbr %f0,%r1");
    test_insn("cegbr %f0,%r1");
    test_insn("cdgbr %f0,%r1");
    test_insn("cxgbr %f0,%r1");
    foreach my $mode (0, 1, 3, 4, 5, 6, 7)
    {
        test_insn("cefbra %f0,$mode,%r1,0", \&mapper);
        test_insn("cdfbra %f0,$mode,%r1,0", \&mapper);
        test_insn("cxfbra %f0,$mode,%r1,0", \&mapper);
        test_insn("cegbra %f0,$mode,%r1,0", \&mapper);
        test_insn("cdgbra %f0,$mode,%r1,0", \&mapper);
        test_insn("cxgbra %f0,$mode,%r1,0", \&mapper);
    }

    header("CONVERT FROM LOGICAL");
    # cdlfbr 32-bit uint -->  64-bit BFP  Iop_I32UtoF64  has no rounding mode
    # cxlfbr 32-bit uint --> 128-bit BFP  Iop_I32UtoF128 has no rounding mode
    # cxlgbr 64-bit uint --> 128-bit BFP  Iop_I64UtoF128 has no rounding mode
    # For those rounding mode 4 is chosen when emitting.
    foreach my $mode (0, 1, 3, 4, 5, 6, 7)
    {
        test_insn("celfbr %f0,$mode,%r1,0", \&mapper);
        test_insn("cdlfbr %f0,$mode,%r1,0", \&mapper);
        test_insn("cxlfbr %f0,$mode,%r1,0", \&mapper);
        test_insn("celgbr %f0,$mode,%r1,0", \&mapper);
        test_insn("cdlgbr %f0,$mode,%r1,0", \&mapper);
        test_insn("cxlgbr %f0,$mode,%r1,0", \&mapper);
    }

    header("CONVERT TO FIXED");
    foreach my $mode (0, 1, 3, 4, 5, 6, 7)
    {
        test_insn("cfebr %r0,$mode,%f1");
        test_insn("cfdbr %r0,$mode,%f1");
        test_insn("cfxbr %r0,$mode,%f1");
        test_insn("cgebr %r0,$mode,%f1");
        test_insn("cgdbr %r0,$mode,%f1");
        test_insn("cgxbr %r0,$mode,%f1");
    }
    foreach my $mode (0, 1, 3, 4, 5, 6, 7)
    {
        test_insn("cfebra %r0,$mode,%f1,0", \&mapper);
        test_insn("cfdbra %r0,$mode,%f1,0", \&mapper);
        test_insn("cfxbra %r0,$mode,%f1,0", \&mapper);
        test_insn("cgebra %r0,$mode,%f1,0", \&mapper);
        test_insn("cgdbra %r0,$mode,%f1,0", \&mapper);
        test_insn("cgxbra %r0,$mode,%f1,0", \&mapper);
    }

    header("CONVERT TO LOGICAL");
    foreach my $mode (0, 1, 3, 4, 5, 6, 7)
    {
        test_insn("clfebr %r0,$mode,%f1,0");
        test_insn("clfdbr %r0,$mode,%f1,0");
        test_insn("clfxbr %r0,$mode,%f1,0");
        test_insn("clgebr %r0,$mode,%f1,0");
        test_insn("clgdbr %r0,$mode,%f1,0");
        test_insn("clgxbr %r0,$mode,%f1,0");
    }

    header("DIVIDE");
    test_insn("debr %f0,%f1");
    test_insn("ddbr %f0,%f1");
    test_insn("dxbr %f0,%f1");

    # echo "DIVIDE TO INTEGER"    not implemented
    # echo "LOAD AND TEST"        insn not mapped one-to-one

    header("LOAD COMPLEMENT");
    test_insn("lcebr %f0,%f1");
    test_insn("lcdbr %f0,%f1");
    test_insn("lcxbr %f0,%f1");

    header("LOAD FP INTEGER");
    foreach my $mode (0, 1, 3, 4, 5, 6, 7)
    {
        test_insn("fiebr %f0,$mode,%f1");
        test_insn("fidbr %f0,$mode,%f1");
        test_insn("fixbr %f0,$mode,%f1");
    }
    foreach my $mode (0, 1, 3, 4, 5, 6, 7)
    {
        test_insn("fiebra %f0,$mode,%f1,0", \&mapper);
        test_insn("fidbra %f0,$mode,%f1,0", \&mapper);
        test_insn("fixbra %f0,$mode,%f1,0", \&mapper);
    }

    header("LOAD LENGTHENED");
    test_insn("ldebr %f0,%f1");
    test_insn("lxdbr %f0,%f1");
    test_insn("lxebr %f0,%f1");
    
    header("LOAD NEGATIVE");
    test_insn("lnebr %f0,%f1");
    test_insn("lndbr %f0,%f1");
    #test_insn("lnxbr %f0,%f1");       insn not mapped one-to-one

    header("LOAD POSITIVE");
    test_insn("lpebr %f0,%f1");
    test_insn("lpdbr %f0,%f1");
    test_insn("lpxbr %f0,%f1");

    header("LOAD ROUNDED");
    test_insn("ledbr %f0,%f1");
    test_insn("ldxbr %f0,%f1");
    test_insn("lexbr %f0,%f1");
    foreach my $mode (0, 1, 3, 4, 5, 6, 7)
    {
        test_insn("ledbra %f0,$mode,%f1,0", \&mapper);
        test_insn("ldxbra %f0,$mode,%f1,0", \&mapper);
        test_insn("lexbra %f0,$mode,%f1,0", \&mapper);
    }

    header("MULTIPLY");
    test_insn("meebr %f0,%f1");
    test_insn("mdbr  %f0,%f1");
    test_insn("mxbr  %f0,%f1");
    # mdebr not implemented
    # mxdbr not implemented

    header("MULTIPLY AND ADD");
    test_insn("maebr %f0,%f1,%f2");
    test_insn("madbr %f0,%f1,%f2");
    
    header("MULTIPLY AND SUBTRACT");
    test_insn("msebr %f0,%f1,%f2");
    test_insn("msdbr %f0,%f1,%f2");
    
    header("SQUARE ROOT");
    test_insn("sqebr %f0,%f1");
    test_insn("sqdbr %f0,%f1");
    test_insn("sqxbr %f0,%f1");

    header("SUBTRACT");
    test_insn("sebr %f0,%f1");
    test_insn("sdbr %f0,%f1");
    test_insn("sxbr %f0,%f1");

    # TEST DATA CLASS          insn not mapped one-to-one
    exit 0;
}

sub mapper
{
    my ($mnm, @opnds) = @_;
    
    my $mapped = $insn_map{$mnm};
    if ($mapped) {
        return $mapped;
    }

    $mapped = $mnm;
    if ($opnds[1] eq "0") {
        if ($mnm eq "cefbra" || 
            $mnm eq "cegbra" || $mnm eq "cdgbra" || 
            $mnm eq "ledbra" || $mnm eq "ldxbra" || $mnm eq "lexbra") {
            $mapped =~ s/a//;
        }
    }
    return $mapped;
}

sub test_insn
{
    my ($insn) = @_;
    my ($mnm, @opnds) = disect_insn($insn);
    print "Testing:  $insn\n";

    my $exe = "xxbfp-$mnm";
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
    my $stdout = `$valgrind $valargs ./$exe 2>&1`;

    # Parse the output from valgrind
    # Need to find the "interesting" insns in the stream. There is exactly
    # one such insn in the "frontend" part and in the "assembly" part.
    #
    # Let
    # - U be the unmodified mnemonic
    # - M be the mapped mnemonic
    # - F be the mnemonic in the "frontend" section
    # - A be the mnemonic in the "frontend" section
    #
    # There are 4 cases to distinguish:
    # 1) F == U and A == U   e.g  aebr
    # 2) F == U and A == M   e.g. kebr  (F == kebr , A == cebr)
    # 3) F == M and A == U   does not occur
    # 4) F == M and A == M   e.g. cefbra with rounding mode == 0 --> cefbr
    #
    my $mapped = $mnm;
    if (scalar @_ == 2) {
        my $mapfunc = $_[1];
        $mapped = $mapfunc->($mnm, @opnds);
    }
    
    my $U = $mnm;
    my $M = $mapped;

    my $output = "";
    my $in_frontend = 0;
    my $in_assembly = 0;
    my $frontend_insn = "";
    my $assembly_insn = "";
    my @lines = split /\n/,$stdout;
    for my $line (@lines) {
        if ($line =~ /Front end/) {
            $in_frontend = 1;
#            $output .= "$line\n";
            next;
        } elsif ($line =~ /Assembly/) {
            $in_frontend = 0;
            $in_assembly = 1;
#            $output .= "$line\n";
            next;
        }
        if ($in_frontend) {
            # insns begin in column #1 and are in lower case
            if ($line =~ /^[a-z]/) {
                my $F = $line;
                $F =~ s/\s.*//;
                if ($F eq $U || $F eq $M) {
                    $output .= "Frontend: $line\n";
                }
            }
            next;
        }
        if ($in_assembly) {
            # Skip v-insns
            next if ($line =~ /^v-/);
            # insns begin in column #1 and are in lower case
            if ($line =~ /^[a-z]/) {
                my $A = $line;
                $A =~ s/\s.*//;
                if ($A eq $U || $A eq $M) {
                    $output .= "Assembly: $line\n";
                }
            }
            next;
        }
    }
    print "$output";

    # Check result
    my $rc = check_valgrind_output($mnm, $output);

    # Remove files 
    if ($rc == 0) {
        unlink ($exe, $cfile, "$exe.s", "$exe.s.orig");
    }
}

sub check_valgrind_output
{
    my ($mnm, $stdout) = @_;

    my @lines = split /\n/,$stdout;
    my $num_lines = scalar @lines;

    if ($num_lines != 2) {
        error("$mnm: Expected 2 lines; found $num_lines");
        for my $line (@lines) {
            print "LINE |$line|\n";
        }
        return 1;
    }
    if ($lines[0] !~ "Frontend") {
        error("$mnm: Unrecognised line |$lines[0]|");
        return 1;
    }
    if ($lines[1] !~ "Assembly") {
        error("$mnm: Unrecognised line |$lines[1]|");
        return 1;
    }
    return 0;
}

sub disect_insn
{
    my ($insn) = @_;
    my ($mnm, $opnd_string) = ($insn, "");

    if ($insn =~ /^([a-zA-Z][a-zA-A0-9]*)\s+(.*)$/) {
        $mnm = $1;
        $opnd_string = $2;
    }

    my @opnds = split /\s*,\s*/, $opnd_string;

    return ($mnm, @opnds);
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
