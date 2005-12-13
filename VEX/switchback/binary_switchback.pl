#!/usr/bin/env perl
use strict;
use warnings;

######################################################
# Binary search script for switchback
# Finds bad basic block for seg faults and bad output.
#
# To test output, you need to create test_ref
# test_ref should hold the correct output for running the test_xxx program:
#  - Everything between (not including) /^---START---$/ and /^---STOP---$/
#  - But NOT including output from /^---begin SWITCHBACK/
#    to /^---  end SWITCHBACK/ inclusive
#
# This script can't handle other vex output,
# so e.g switchback.c::DEBUG_TRACE_FLAGS should be 0
#

######################################################
# Global consts, vars
use constant DEBUG => 0;
use constant CONST_N_MAX => 10000000000;
use constant CONST_N_MUL => 2;

my $SWITCHBACK = "./switchback";
my $N_START = 0;
my $N_LAST_GOOD = 0;
my $N_LAST_BAD = -1;
my $GIVEN_LAST_GOOD = -1;
my $GIVEN_LAST_BAD = -1;
my $TEST_REF;



######################################################
# Helper functions

sub Exit {
    exit $_[0];
}

sub Usage {
    print "Usage: binary_switchback.pl test_ref [last_good [last_bad]]\n";
    print "where:\n";
    print "   test_ref  = reference output from test_xxx\n";
    print "   last_good = last known good bb (search space minimum)\n";
    print "   last_bad  = last known bad bb (search space maximum)\n";
    print "\n";
}

sub QuitUsage {
    print $_[0]."\n";
    Usage();
    Exit 1;
}


######################################################
# Get & check cmdline args
# - if given, override global vars.

if (@ARGV < 1 || @ARGV > 3) {
    QuitUsage "Error: Bad num args\n";
}

$TEST_REF = $ARGV[0];

if ( ! -x "$SWITCHBACK" ) {
    QuitUsage "File doesn't exist | not executable: '$SWITCHBACK'\n";
}

if (@ARGV >1) {
    $N_LAST_GOOD = $ARGV[1];
    $GIVEN_LAST_GOOD = $N_LAST_GOOD;
    if (! ($N_LAST_GOOD =~ /^\d*$/)) {
	QuitUsage "Error: bad arg for #last_good\n";
    }
    if ($N_LAST_GOOD >= CONST_N_MAX) {
	QuitUsage "Error: #last_good >= N_MAX(".CONST_N_MAX.")\n";
    }
}
if (@ARGV >2) {
    $N_LAST_BAD = $ARGV[2];
    $GIVEN_LAST_BAD = $N_LAST_BAD;
    if (! ($N_LAST_BAD =~ /^\d*$/)) {
	QuitUsage "Error: bad arg for 'last_bad'\n";
    }
}

# Setup N_START
if ($N_LAST_BAD != -1) {
    # Start halfway:
    my $diff = $N_LAST_BAD - $N_LAST_GOOD;
    $N_START = $N_LAST_GOOD + ($diff - ($diff % 2)) / 2;
} else {
    # No known end: Start at beginning:
    if ($N_LAST_GOOD > 0) {   # User-given last_good
	$N_START = $N_LAST_GOOD;
    } else {
	$N_START = 100;       # Some reasonable number.
    }
}

######################################################
# Sanity checks (shouldn't ever happen)

if ($N_START < $N_LAST_GOOD) {
    print "Program Error: start < last_good\n";
    exit 1;
}
if ($N_LAST_BAD != -1 && $N_START >= $N_LAST_BAD) {
    print "Program Error: start >= last_bad\n";
    exit 1;
}
if ($N_START < 1 || $N_START > CONST_N_MAX) {
    print "Program Error: Bad N_START: '$N_START'\n";
    exit 1;
}
if ($N_LAST_GOOD < 0 || $N_LAST_GOOD > CONST_N_MAX) {
    print "Program Error: Bad N_LAST_GOOD: '$N_LAST_GOOD'\n";
    exit 1;
}
if ($N_LAST_BAD < -1 || $N_LAST_BAD > CONST_N_MAX) {
    print "Program Error: Bad N_LAST_BAD: '$N_LAST_BAD'\n";
    exit 1;
}






######################################################
# Helper functions

# Run switchback for test, for N bbs
# returns output results
sub SwitchBack {
    my $n = $_[0];
    if ($n < 0 || $n > CONST_N_MAX) {
	print "Error SwitchBack: Bad N: '$n'\n";
	Exit 1;
    }
    my $TMPFILE = ".switchback_output.$n";

    print "=== Calling switchback for bb $n ===\n";

    system("$SWITCHBACK $n >& $TMPFILE");
    my $ret = $?;

    if ($ret == 256) {
	print "Error running switchback - Quitting...\n---\n";
	open(INFILE, "$TMPFILE");
	print <INFILE>;
	close(INFILE);

	unlink($TMPFILE) if (! DEBUG);
	exit 0;	
    } 

    if ($ret & 127) {
	print "Ctrl-C pressed - Quitting...\n";
	unlink($TMPFILE) if (! DEBUG);
	exit 0;
    }

    if (DEBUG) {
	if ($ret == -1) {
	    print "failed to execute: $!\n";
	}
	elsif ($ret & 127) {
	    printf "child died with signal %d, %s coredump\n",
            ($ret & 127),  ($ret & 128) ? 'with' : 'without';
	}
	else {
	    printf "child exited with value %d\n", $ret >> 8;
	}
    }
    if ($ret != 0) { # Err: maybe seg fault
	open(INFILE, "$TMPFILE");
	my @results = <INFILE>;
	close(INFILE);

	while (@results && !((shift @results) =~ /^---START---/)) {}
	print @results;

	unlink($TMPFILE) if (! DEBUG);
	return;
    }

    open(INFILE, "$TMPFILE");
    my @results = <INFILE>;
    close(INFILE);

    unlink($TMPFILE) if (! DEBUG);
    return @results;
}

# Returns N simulated bbs from output lines
sub get_N_simulated {
    my @lines = @{$_[0]};
    pop @lines;             # not the first...
    my $line = pop @lines;  # ...but the second line.

    chomp $line;
    my $n;
    if (($n) = ($line =~ /^(\d*) bbs simulated$/)) {
	return $n;
    }
    print "Error: Didn't find N bbs simultated, from output lines\n";
    Exit 1;
}

# Calls test script to compare current output lines with a reference.
# Returns 1 on success, 0 on failure
sub TestOutput {
    my @lines = @{$_[0]};
    my $n = $_[1];
    my $ref_output = "$TEST_REF";

    # Get the current section we want to compare:
    my @newlines;
    my $ok=0;
    my $halfline = "";
    foreach my $line(@lines) {
	chomp $line;
	if ($line =~ /^---STOP---$/) { last; }     # we're done

	# output might be messed up here...
	if ($line =~ /^.*---begin SWITCHBACK/) {
	    ($halfline) = ($line =~ /^(.*)---begin SWITCHBACK/);
	    $ok = 0;  # stop on prev line
	}

	# A valid line:
	if ($ok) {
	    if ($halfline ne "") {   # Fix broken line
		$line = $halfline.$line;
		$halfline = "";
	    }

	    # Ignore Vex output
	    if ($line =~ /^vex /) { next; }

	    push(@newlines, $line);
	}

	if ($line =~ /^---START---$/) {            # start on next line
	    $ok = 1;
	}

	if ($line =~ /^---  end SWITCHBACK/) {     # start on next line
	    $ok = 1;
	    
	}
    }

    if (DEBUG) {
	open(OUTFILE, ">.filtered_output.$n");
	print OUTFILE join("\n",@newlines);
	close(OUTFILE);
    }

    # Read in reference lines
    open(REFERENCE, "$ref_output") || die "Error: Couldn't open $ref_output\n";
    my @ref_lines = <REFERENCE>;
    close(REFERENCE);

    # Compare reference lines with current:
    my $match = 1;
    my $i = 0;
    foreach my $ref_line(@ref_lines) {
	chomp $ref_line;
	my $line = $newlines[$i++];
	chomp $line;
	if ($ref_line ne $line) {
	    print "\nMismatch on output:\n";
	    print "ref: '$ref_line'\n";
	    print "new: '$line'\n\n";
	    $match = 0;
	    last;
	}
    }
    return $match;
}






######################################################
# Do the search

if (DEBUG) {
    print "\n------------\n";
    print "START:  N=$N_START\n";
    print "START: lg=$N_LAST_GOOD\n";
    print "START: lb=$N_LAST_BAD\n";
    print "START: GIVEN_LAST_GOOD=$GIVEN_LAST_GOOD\n";
    print "START: GIVEN_LAST_BAD =$GIVEN_LAST_BAD\n";
    print "\n";
}

my $N = $N_START;
my $success = 0;
my @sb_output;
while (1) {
    if (DEBUG) {
	print "\n------------\n";
	print "SOL: lg=$N_LAST_GOOD\n";
	print "SOL: lb=$N_LAST_BAD\n";
	print "SOL:  N=$N\n";
    }
    if ($N < 0) {
	print "Error: $N<0\n";
	Exit 1;
    }

    my $ok = 1;
    # Run switchback:
    @sb_output = SwitchBack($N);

    if (@sb_output == 0) { # Switchback failed - maybe seg fault
	$ok = 0;
    }

    if (DEBUG) {
	open(fileOUT, ">.retrieved_output.$N") or die("Can't open file for writing: $!");
	print fileOUT @sb_output;
	close(fileOUT);
    }

    # If we're ok so far (no seg faults) then test for correct output
    if ($ok) {
	$ok = TestOutput( \@sb_output, $N );
    }

    if ($ok) {
	if (get_N_simulated(\@sb_output) < $N) { # Done: No bad bbs
	    $success = 1;
	    last;
	}
	if ($N_LAST_BAD == -1) {
	    # No upper bound for search space
	    # Try again with a bigger N

	    $N_LAST_GOOD = $N;
	    $N *= CONST_N_MUL;
	    if ($N > CONST_N_MAX) {
		print "\nError: Maxed out N($N): N_MAX=".CONST_N_MAX."\n";
		print "\nWe're either in a loop, or this is a big test program (increase N_MAX)\n\n";
		Exit 1;
	    }
	    if (DEBUG) {
		print "Looks good so far: Trying bigger N...\n\n";
	    }
	    next;
	}
    }

    # Narrow the search space:
    if ($ok) { $N_LAST_GOOD = $N; }
    else {     $N_LAST_BAD  = $N;  }

    # Calculate next step:
    my $diff = $N_LAST_BAD - $N_LAST_GOOD;
    $diff = $diff - ($diff % 2);
    my $step = $diff / 2;

    if ($step < 0) {
	print "Error: step = $step\n";
	Exit 1;
    }

    # This our last run-through?
    if ($step!=0) {
	$N = $N_LAST_GOOD + $step;   # Keep on going...
    } else {
	last;                        # Get outta here
    }

    if (DEBUG) {
	print "\nEOL: ok=$ok\n";
	print "EOL: lg=$N_LAST_GOOD\n";
	print "EOL: lb=$N_LAST_BAD\n";
	print "EOL:  s=$step\n";
	print "EOL:  N=$N\n";
    }
}



######################################################
# Done: Report results

print "\n============================================\n";
print "Done searching.\n\n";

if ($N_LAST_BAD != -1 && $N != $N_LAST_BAD) {
    print "Getting output for last bad bb:\n";
    @sb_output = SwitchBack($N_LAST_BAD);
}

print @sb_output;
print "\n\n";
if ($success) {
    print "*** Success!  No bad bbs found. ***\n";
} else {
    if ($N_LAST_BAD == $GIVEN_LAST_BAD) {
	print "*** No failures detected within given bb range ***\n";
	print " - check given 'last_bad' argument\n";
    } else {
	if ($N_LAST_BAD == $GIVEN_LAST_GOOD) {
	    print "*** Failed on bb given as last_good ***\n";
	    print " - decrease the 'last_good' argument\n";
	} else {
	    print "*** Failure: Last failed switchback bb: $N_LAST_BAD ***\n";
	    print "Hence bad bb: ". ($N_LAST_BAD - 1) ."\n";
	}
    }
}
print "\n";
if (DEBUG) {
    print "END:  N=$N\n";
    print "END: lg=$N_LAST_GOOD\n";
    print "END: lb=$N_LAST_BAD\n";
    print "END: GIVEN_LAST_BAD=$GIVEN_LAST_BAD\n";
    print "\n";
}
Exit 0;
