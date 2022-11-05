INTRO
-----
This directory (nightly/) contains a simple, automatic build-and-test
system for Valgrind, intended to be run nightly by cron or a similar
program.


BASIC OPERATIONS
----------------
When run, the system clones and checks out two trees: one from 24 hours ago
and the HEAD from now.  ("24 hours ago" and "now" are determined when
the script starts running, so if any commits happen while the tests are
running they will not be tested.)

If the two trees are different (i.e. there have been commits in the past 24
hours, either to the master or a branch) it builds ("make"), installs ("make
install") and runs the regression tests ("make regtest") in both, and
compares the results.  Note that the "make install" isn't necessary in order
to run the tests because the regression tests use the code built (with
"make") within the tree, but it's worth doing because it tests that "make
install" isn't totally broken.  After checking both trees, it emails a
summary of the results to a recipient.  All this typically takes something
like 30 minutes.

If the two trees are identical, the tests are not run and no results are
emailed.  This avoids spamming people with uninteresting results emails when
no commits have happened recently.


SETTING UP
----------
To set up nightly testing for a machine, do the following.

(1) Clone a shallow copy from the repository, eg:

        git clone --depth 1 https://sourceware.org/git/valgrind.git/ $DIR

    where $DIR is the name of the directory you want it to be in.
    You'll probably want to include "nightly" in the directory name.
    
    Note that this doesn't clone the whole repo history, just the latest
    revision of everything in the repository. Under $DIR, only directory
    "nightly" is of interest to us - the rest is ignored. In other words: the
    testing script clones and checks out new trees (within $DIR/nightly) and
    tests them independently.

(2) Choose a tag that identifies the test results.  This is usually the
    machine name.  We'll call it $TAG in what follows.

(3) Create a configuration file $DIR/nightly/conf/$TAG.conf. It is sourced
    by the 'nightly' script, and can define any or all of the following
    environment variables. (In most cases, only ABT_DETAILS is needed.)

    - ABT_DETAILS: describes the machine in more detail, eg. the OS.  The
      default is empty, but you should define it.  An example:

        export ABT_DETAILS="Ubuntu 9.04, Intel x86-64"

      You could also use some invocation of 'uname' or something similar
      to generate this string.  Eg. on Ubuntu Linux this works nicely:

        export ABT_DETAILS="`cat /etc/issue.net`, `uname -m`"

      And on Mac OS X this works nicely:

        export ABT_DETAILS=`uname -mrs`

      The advantage of doing it like this is that if you update the OS on
      the test machine you won't have to update ABT_DETAILS manually.

    - ABT_CONFIGURE_OPTIONS: gives extra configure options.  The default is
      empty.

    - ABT_EVAL: if provided, it must be the name of a shell script that
      executes the shell command $1 with arguments $2 .. ${$#}.  Allows to
      compile and run the Valgrind regression tests on another system than
      the system the 'nightly' script runs on.  It is assumed that the remote
      system shares the local filesystem tree through e.g. NFS.  It is the
      responsibility of the shell script to set the remote working directory
      such that it matches the local current directory ($PWD).

    - ABT_RUN_REGTEST: if provided, it must be the name of an argumentless
      shell function (also specified in the $TAG.conf file) that will be used
      to run the tests.  If not specified, the usual "make regtest" will be
      used.

    - ABT_JOBS: allows parallel builds -- it's passed as the argument to
      "make -j" when building Valgrind and the tests.  The default is 1.

    - ABT_PERF: unset or set to "" mean 'do not run perf tests' (default value)
                set to "--vg=../valgrind-new" (run perf tests for new tree)
                set to "--vg=../valgrind-new --vg=../valgrind-old"
                (run  perf tests for "new" and for "24 hours ago",
                 to compare the performances between the 2 trees).
      
    - ABT_PERF_TOOLS: --tools=.... option of perf/vg_perf.
      (default value: all non experimental tools)

    - ABT_PERF_REPS: --reps=... option of perf/vg_perf
      (default value: --reps=3)

    Note that the appropriate syntax to use in this file will depend on the
    shell from which the $DIR/nightly/bin/nightly script is run (which in turn
    may depend on what shell is used by cron or any similar program).

(4) Create a mailer script $DIR/nightly/conf/$TAG.sendmail. It must be
    executable. It's used to send email results to the desired recipient (e.g.
    valgrind-developers@lists.sourceforge.net). It must handle three command
    line arguments.

    - The first argument is the email subject line.  It contains
      $ABT_DETAILS plus some other stuff.
      
    - The second argument is the name of the file containing the email's
      body (which shows the tests that failed, and the differences between now
      and 24 hours ago). 
      
    - The third is the name of the file containing all the diffs from
      failing tests.  Depending on the test results you get, you could
      inline this file into the email body, or attach it, or compress and
      attach it, or even omit it.  The right choice depends on how many
      failures you typically get -- if you get few failures, inlining the
      results make them easier to read;  if you get many failures,
      compressing might be a good idea to minimise the size of the emails.

    The best way to do this depends on how mail is set up on your machine.
    You might be able to use /usr/bin/mail, or you might need something more
    elaborate like using Mutt to send mail via an external account.

    At first, you should probably just send emails to yourself for testing
    purposes.  After it's working, then sending it to others might be
    appropriate.

(5) To run the tests, execute:

       $DIR/nightly/bin/nightly $DIR/nightly $TAG

    You probably want to put this command into a cron file or equivalent
    so it is run regularly (preferably every night).  Actually, it's
    probably better to put that command inside a script, and run the script
    from cron, rather than running $DIR/nightly/bin/nightly directly. That way
    you can put any other configuration stuff that's necessary inside the
    script (e.g. make sure that programs used by the mailer script are in
    your PATH).


OUTPUT FILES
------------
If the tests are run, the following files are produced:

- $DIR/nightly/old.verbose and $DIR/nightly/new.verbose contain full output of
  the whole process for each of the two trees.

- $DIR/nightly/old.short and $DIR/nightly/new.short contain summary output of
  the process for each of the two trees. The diff between these two files goes
  in $DIR/nightly/diff.short.

- $DIR/nighlty/final contains the overall summary, constructed from
  $DIR/nightly/old.short, $DIR/nightly/new.short, $DIR/nightly/diff.short and
  some other bits and pieces. (The name of this file is what's passed as the
  second argument to $DIR/nightly/conf/$TAG.sendmail.)

- $DIR/nightly/diffs holds the diffs from all the failing tests in the newer
  tree, concatenated together; the diff from each failure is truncated at 100
  lines to minimise possible size blow-outs. (The name of this file is
  what's passed as the third argument to $DIR/nightly/conf/$TAG.sendmail.)

- $DIR/nightly/sendmail.log contains the output (stdout and stderr) from
  $DIR/nightly/conf/$TAG.sendmail goes in $DIR/nightly/sendmail.log.

- $DIR/nightly/valgrind-old/ and $DIR/nightly/valgrind-new/ contain the tested
  trees (and $DIR/nightly/valgrind-old/Inst/ and $DIR/nightly/valgrind-new/Inst/
  contain the installed code).

If the tests aren't run, the following file is produced:

- $DIR/nightly/unchanged.log is created only if no tests were run because the
  two trees were identical. It will contain a short explanatory message.

Each time the tests are run, all files from previous runs are deleted.


TROUBLESHOOTING
---------------
If something goes wrong, looking at the output files can be useful.  For
example, if no email was sent but you expected one, check sendmail.log to
see if the mailer script had a problem.  Or check if unchanged.log exists.

Occasionally the git server isn't available when the tests runs, for either
or both trees.  When this happens the email will be sent but it won't be
very informative.  Usually it's just a temporary server problem and it'll
run fine the next time without you having to do anything.

Note that the test suite is imperfect:  
- There are very few machines where all tests pass;  that's why the old/new
  diff is produced.  Some of the tests may not be as portable as intended.
- Some tests are non-deterministic, and so may pass one day and fail the
  next.

Improving the test suite to avoid these problems is a long-term goal but it
isn't easy.


MAINTENANCE
-----------
The scripts in the nightly/ directory occasionally get updated.  If that
happens, you can just "git pull" within $DIR to get the updated versions,
which will then be used the next time the tests run.  (It's possible that
the scripts will be changed in a way that requires changes to the files in
$DIR/nightly/conf/, but we try to avoid this.)

If you want such updates to happen automatically, you could write a script
that does all the steps in SETTING UP above, and instead run that script
from cron.
