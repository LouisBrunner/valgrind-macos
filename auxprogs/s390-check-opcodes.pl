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
my %toir_decoded = ();
my %known_arch = map {($_ => 1)}
    qw(g5 z900 z990 z9-109 z9-ec z10 z196 zEC12 z13 arch12 arch13 arch14);

# Patterns for identifying certain extended mnemonics that shall be
# skipped in "s390-opc.txt" and "s390-opcodes.csv".

my @extended_mnemonics = (
    "bi",			# extended mnemonic for bic
    'brul?',
    'jasl?',
    'jctg?',
    'jg?nop',
    'jxleg?',
    'jxhg?',
    'l[de]rv',
    'risbgn?z',
    'st[de]rv',
    "va[bhfgq]",
    "vacc[bhfgq]",
    "vacccq",
    "vacq",
    "vavgl*[bhfg]",
    "vcdl*gb",
    'vcfp[sl]',
    '[vw]cel?fb',
    'vc[sl]fp',
    '[vw]cl?feb',
    "vceq[bhfg]s*",
    "vchl*[bhfg]s*",
    "vcl*gdb",
    "vc[lt]z[bhfg]",
    "vecl*[bhfg]",
    "verim[bhfg]",
    "verllv*[bhfg]",
    "veslv*[bhfg]",
    "vesrav*[bhfg]",
    "vesrlv*[bhfg]",
    "vfaez*[bhfg]s*",
    "vfeez*[bhfg]s*",
    "vfenez*[bhfg]s*",
    "vfce[sd]bs*",
    "vfchdbs*",
    "vfche[sd]bs*",
    "vfchsbs*",
    "vfd[sd]b",
    "vfa[sd]b",
    "vfi[sd]b",
    "vfke[sd]bs*",
    "vfkhe*[sd]bs*",
    "vflc[sd]b",
    "vfll[sd]",
    "[vw]flr[dx]",
    "vfl[np][sd]b",
    "vfm[as]*[sd]b",
    "vfmax[sd]b",
    "vfmin[sd]b",
    "vfnm[as][sd]b",
    "vfpso[sd]b",
    "vfsq*[sd]b",
    "vftci[sd]b",
    "vgfma*[bhfg]",
    "vgm[bhfg]",
    "vistr[bhfg]s*",
    'vlbr[hfgq]',
    'vlbrrep[hfg]',
    "vlc[bhfg]",
    "[vw]ldeb",
    "[vw]ledb",
    'vler[hfg]',
    "vlgv[bhfg]",
    'vllebrz[hfge]',
    "vllez[bhfg]",
    "vllezlf",
    "vlp[bhfg]",
    "vlrep[bhfg]",
    "vlvg[bhfg]",
    "vmal?[eoh][bhfg]",
    "vmal(b|hw|f)",
    "vml(b|hw|f)",
    "vml?(o|e)[bhf]",
    "vml?h[bhf]",
    "vm[nx]l*[bhfg]",
    "vmr[lh][bhfg]",
    "vmslg",
    "vnot",
    "(vone|vzero)",
    "vpkl*[bhfg]",
    "vpkl*s*[bhfg]s*",
    "vpopct[bhfg]",
    "vrepi*[bhgf]",
    "vs[bhfgq]",
    "vsbcbiq",
    "vsbiq",
    "vscbi[bhfgq]",
    "vseg[bfh]",
    'vstbr[hfgq]',
    'vster[hfg]',
    "vstrcz*[bhf]s*",
    'vstrsz?[bhf]',
    "vsum(b|gh|gf|h|qf|qg)",
    "vuplh[bhf]",
    "vuph[bhf]",
    "vupl(b|hw|f)",
    "vupll[bhf]",
    "wcdl*gb",
    "wcl*gdb",
    "wfa[sdx]b",
    "wfch*e*[sdx]bs*",
    "wf[cdi][sdx]b",
    "wfkh*e*[sdx]bs*",
    "wfk[sdx]b",
    "wfl[clnp][sdx]b*",
    "wfmax[sdx]b",
    "wfmin[sdx]b",
    "wfm[as]*[sdx]b",
    "wfnm[as][sdx]b",
    "wfpso[sdx]b",
    "wftci[sdx]b",
    "wfsq*[sdx]b",
    "vl(ed|de)",
    "prno"			# alternate mnemonic for ppno
    );

# Compile excluded mnemonics into one regular expression to optimize
# speed.  Also it simplifies the code.

my $extended_mnemonics_pattern = '^(' .
    join('|', map "$_", @extended_mnemonics) . ')$';

#----------------------------------------------------
# Read s390-opc.txt (binutils)
#----------------------------------------------------
open(OPC, "$opc_file") || die "cannot open $opc_file\n";
while (my $line = <OPC>) {
    chomp $line;
    next if ($line =~ "^[ ]*#");   # comments
    next if ($line =~ /^\s*$/);    # blank line
    my ($encoding,$mnemonic,$format) = $line =~ /^(\S+) (\S+) (\S+)/gc;

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

    next if ($mnemonic eq "cfdbra"); # indistinguishable from cfdbr
    next if ($mnemonic eq "cfebra"); # indistinguishable from cfebr
    next if ($mnemonic eq "cfxbra"); # indistinguishable from cfxbr
    next if ($mnemonic eq "cgdbra"); # indistinguishable from cgdbr
    next if ($mnemonic eq "cgebra"); # indistinguishable from cgebr
    next if ($mnemonic eq "cgxbra"); # indistinguishable from cgxbr
    next if ($mnemonic eq "cdfbra"); # indistinguishable from cdfbr
    next if ($mnemonic eq "cefbra"); # indistinguishable from cefbr
    next if ($mnemonic eq "cxfbra"); # indistinguishable from cxfbr
    next if ($mnemonic eq "cdgbra"); # indistinguishable from cdgbr
    next if ($mnemonic eq "cegbra"); # indistinguishable from cegbr
    next if ($mnemonic eq "cxgbra"); # indistinguishable from cxgbr
    next if ($mnemonic eq "ldxbra"); # indistinguishable from ldxbr
    next if ($mnemonic eq "lexbra"); # indistinguishable from lexbr
    next if ($mnemonic eq "ledbra"); # indistinguishable from ledbr
    next if ($mnemonic eq "cdgtr");  # indistinguishable from cdgtra
    next if ($mnemonic eq "cxgtra"); # indistinguishable from cxgtr
    next if ($mnemonic eq "cgdtra"); # indistinguishable from cgdtr
    next if ($mnemonic eq "cgxtra"); # indistinguishable from cgxtr
    next if ($mnemonic eq "fidbr");  # indistinguishable from fidbra
    next if ($mnemonic eq "fiebr");  # indistinguishable from fiebra
    next if ($mnemonic eq "fixbr");  # indistinguishable from fixbra
    next if ($mnemonic eq "adtr");   # indistinguishable from adtra
    next if ($mnemonic eq "axtr");   # indistinguishable from axtra
    next if ($mnemonic eq "sdtr");   # indistinguishable from sdtra
    next if ($mnemonic eq "sxtr");   # indistinguishable from sxtra
    next if ($mnemonic eq "ddtr");   # indistinguishable from ddtra
    next if ($mnemonic eq "dxtr");   # indistinguishable from dxtra
    next if ($mnemonic eq "mdtr");   # indistinguishable from mdtra
    next if ($mnemonic eq "mxtr");   # indistinguishable from mxtra
    next if ($mnemonic =~ /$extended_mnemonics_pattern/);

    my ($description) = $line =~ /\G\s+"\s*(.*?)\s*"/gc;
    my ($arch) = $line =~ /\G\s+(\S+)/gc;
    unless ($known_arch{$arch}) {
	unless (exists $known_arch{$arch}) {
	    print "warning: unsupported arch \"$arch\" in s390-opc.txt\n";
	    $known_arch{$arch} = 0;
	}
	next;
    }

    $description =~ s/\s\s+/ /g; # replace multiple blanks with a single one

    # Certain opcodes are listed more than once. Let the first description
    # win.
    if (exists $opc_desc{$mnemonic}) {
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

    next if ($mnemonic eq "cfdbra"); # indistinguishable from cfdbr
    next if ($mnemonic eq "cfebra"); # indistinguishable from cfebr
    next if ($mnemonic eq "cfxbra"); # indistinguishable from cfxbr
    next if ($mnemonic eq "cgdbra"); # indistinguishable from cgdbr
    next if ($mnemonic eq "cgebra"); # indistinguishable from cgebr
    next if ($mnemonic eq "cgxbra"); # indistinguishable from cgxbr
    next if ($mnemonic eq "cdfbra"); # indistinguishable from cdfbr
    next if ($mnemonic eq "cefbra"); # indistinguishable from cefbr
    next if ($mnemonic eq "cxfbra"); # indistinguishable from cxfbr
    next if ($mnemonic eq "cegbra"); # indistinguishable from cegbr
    next if ($mnemonic eq "cdgbra"); # indistinguishable from cdgbr
    next if ($mnemonic eq "cegbra"); # indistinguishable from cegbr
    next if ($mnemonic eq "cxgbra"); # indistinguishable from cxgbr
    next if ($mnemonic eq "ldxbra"); # indistinguishable from ldxbr
    next if ($mnemonic eq "lexbra"); # indistinguishable from lexbr
    next if ($mnemonic eq "ledbra"); # indistinguishable from ledbr
    next if ($mnemonic eq "cdgtr");  # indistinguishable from cdgtra
    next if ($mnemonic eq "cxgtra"); # indistinguishable from cxgtr
    next if ($mnemonic eq "cgdtra"); # indistinguishable from cgdtr
    next if ($mnemonic eq "cgxtra"); # indistinguishable from cgxtr
    next if ($mnemonic eq "fidbr");  # indistinguishable from fidbra
    next if ($mnemonic eq "fiebr");  # indistinguishable from fiebra
    next if ($mnemonic eq "fixbr");  # indistinguishable from fixbra
    next if ($mnemonic eq "adtr");   # indistinguishable from adtra
    next if ($mnemonic eq "sdtr");   # indistinguishable from sdtra
    next if ($mnemonic eq "ddtr");   # indistinguishable from ddtra
    next if ($mnemonic eq "mdtr");   # indistinguishable from mdtra
    next if ($mnemonic =~ /$extended_mnemonics_pattern/);

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
    if ($line =~ /goto\s+unimplemented/) {
	# Assume this is in the decoder
	if ($line =~ /\/\*\s([A-Z][A-Z0-9]*)\s\*\//) {
	    my $mnemonic = lc $1;
	    $toir_decoded{$mnemonic} = 1;
	}
    } elsif ($line =~ /^s390_irgen_([A-Z][A-Z0-9]*)\b/) {
	my $mnemonic = lc $1;
	$toir_implemented{$mnemonic} = 1;
    }
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
    if (defined $csv_desc{$opc}) {
	if ($opc_desc{$opc} ne $csv_desc{$opc}) {
	    print "*** opcode $opc differs:\n";
	print "    binutils:    $opc_desc{$opc}\n";
	    print "    opcodes.csv: $csv_desc{$opc}\n";
	}
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

#----------------------------------------------------
# 4) Make sure all opcodes are handled by the decoder
#----------------------------------------------------

# We only have to check those for which we don't generate IR.

foreach my $opc (keys %opc_desc) {
    if (! $toir_implemented{$opc} && ! $toir_decoded{$opc}) {
	print "*** opcode $opc is not handled by the decoder\n";
    }
}

print "there are " . int(keys %toir_implemented) . " implemented opcodes\n";
exit 0
