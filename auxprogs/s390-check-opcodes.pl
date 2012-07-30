#!/usr/bin/env perl

use strict;
use warnings;

#------------------------------------------------------------------
# This script assists in updating s390-opcodes.csv
# It utilizes <binutils>/opcodes/s390-opc.txt and
# <valgrind>/VEX/priv/guest_s390_toIR.c and will
# - identify new opcodes that are present in s390-opc.txt
#   (s390-opc.txt is the golden list)
# - identify opcodes that are implemented in guest_s390_toIR.c
#   but have an out-of-date status in the CSV file.
#------------------------------------------------------------------
my $num_arg = $#ARGV + 1;

if ($num_arg != 3) {
    die "usage: s390-check-opcodes s390-opcodes.csv s390-opc.txt guest_s390_toIR.c\n";
}

my $csv_file  = $ARGV[0];
my $opc_file  = $ARGV[1];
my $toir_file = $ARGV[2];

my %opc_desc = ();
my %csv_desc = ();
my %csv_implemented = ();
my %toir_implemented = ();


#----------------------------------------------------
# Read s390-opc.txt (binutils)
#----------------------------------------------------
open(OPC, "$opc_file") || die "cannot open $opc_file\n";
while (my $line = <OPC>) {
    chomp $line;
    next if ($line =~ "^[ ]*#");   # comments
    my $description = (split /"/,$line)[1];
    my ($encoding,$mnemonic,$format) = split /\s+/,$line;

    # Ignore opcodes that have wildcards in them ('$', '*')
    # Those provide alternate mnemonics for specific instances of this opcode
    next if ($mnemonic =~ /\$/);
    next if ($mnemonic =~ /\*/);

    # Ignore certain opcodes which are special cases of other opcodes
    next if ($mnemonic eq "br");    # special case of bcr
    next if ($mnemonic eq "nopr");  # special case of bcr
    next if ($mnemonic eq "b");     # special case of bc
    next if ($mnemonic eq "nop");   # special case of bc
    next if ($mnemonic eq "j");     # special case of brc
    next if ($mnemonic eq "jg");    # special case of brcl
    next if ($mnemonic eq "tmh");   # alternate mnemonic for tmlh
    next if ($mnemonic eq "tml");   # alternate mnemonic for tmll
    next if ($mnemonic eq "lrdr");  # alternate mnemonic for ldxr
    next if ($mnemonic eq "lrer");  # alternate mnemonic for ledr
    next if ($mnemonic eq "me");    # alternate mnemonic for mde
    next if ($mnemonic eq "mer");   # alternate mnemonic for mder
    next if ($mnemonic eq "cuutf"); # alternate mnemonic for cu21
    next if ($mnemonic eq "cutfu"); # alternate mnemonic for cu12

    $description =~ s/^[\s]+//g;    # remove leading blanks
    $description =~ s/[\s]+$//g;    # remove trailing blanks
    $description =~ s/[ ][ ]+/ /g;  # replace multiple blanks with a single one


# Certain opcodes are listed more than once. Let the first description win
    if ($opc_desc{$mnemonic}) {
        # already there
#        if ($opc_desc{$mnemonic} ne $description) {
#            print "multiple description for opcode $mnemonic\n";
#            print "  old: |" . $opc_desc{$mnemonic} . "|\n";
#            print "  new: |" . $description . "|\n";
#        }
    } else {
        $opc_desc{$mnemonic} = $description;
    }

    if ($description =~ /,/) {
        print "warning: description of $mnemonic contains comma\n";
    }
}
close(OPC);

#----------------------------------------------------
# Read CSV file (valgrind)
#----------------------------------------------------
open(CSV, "$csv_file") || die "cannot open $csv_file\n";
while (my $line = <CSV>) {
    chomp $line;
    next if ($line =~ "^[ ]*#");   # comments
    my ($mnemonic,$description,$status) = split /,/,$line;

    $mnemonic    =~ s/"//g;
    $description =~ s/"//g;

# Complain about duplicate entries. We don't want them.
    if ($csv_desc{$mnemonic}) {
        print "$mnemonic: duplicate entry\n";
    } else {
        $csv_desc{$mnemonic} = $description;
    }
# Remember whether it is implemented or not
    next if ($line =~ /not\s+implemented/);
    next if ($line =~ /N\/A/);
    next if ($line =~ /won't do/);
    if ($line =~ /implemented/) {
        $csv_implemented{$mnemonic} = 1;
    } else {
        print "*** unknown implementation status of $mnemonic\n";
    }
}
close(CSV);

#----------------------------------------------------
# Read s390_guest_toIR.c file. Compile list of implemented opcodes
#----------------------------------------------------
open(TOIR, "$toir_file") || die "cannot open $toir_file\n";
while (my $line = <TOIR>) {
    chomp $line;
    next if (! ($line =~ /^s390_irgen_[A-Z]/));
    $line =~ /^s390_irgen_([A-Z][A-Z0-9]*)/;
    my $op = $1;
    $op =~ tr/A-Z/a-z/;
    $toir_implemented{$op} = 1;
}
close(TOIR);

#----------------------------------------------------
# 1) Make sure there are no missing/extra opcodes
#----------------------------------------------------
foreach my $opc (keys %opc_desc) {
    if (! $csv_desc{$opc}) {
        print "*** opcode $opc not listed in $csv_file\n";
    }
}
foreach my $opc (keys %csv_desc) {
    if (! $opc_desc{$opc}) {
        print "*** opcode $opc not listed in $opc_file\n";
    }
}

#----------------------------------------------------
# 2) Make sure opcode descriptions are the same
#----------------------------------------------------
foreach my $opc (keys %opc_desc) {
    if ($opc_desc{$opc} ne $csv_desc{$opc}) {
        print "*** opcode $opc differs: $opc_desc{$opc}\n";
    }
}

#----------------------------------------------------
# 3) Make sure implemented'ness is correct
#----------------------------------------------------
foreach my $opc (keys %toir_implemented) {
    if (! $csv_implemented{$opc}) {
        print "*** opcode $opc is implemented but CSV file does not say so\n";
    }
}

foreach my $opc (keys %csv_implemented) {
    if (! $toir_implemented{$opc}) {
        print "*** opcode $opc is not implemented but CSV file says so\n";
    }
}

print "there are " . int(keys %toir_implemented) . " implemented opcodes\n";
exit 0
