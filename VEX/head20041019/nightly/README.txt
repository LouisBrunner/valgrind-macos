
This directory (nightly/) contains a simple, automatic build-and-test
system for Valgrind, intended to be run by cron.

Note (importantly) it doesn't test the sources in the tree of which
this directory is a part (viz, nightly/..).  Instead it checks out
a complete new tree, builds and tests that independently of the
existing tree.

To use, choose a tag, probably a machine name, and run

   bin/nightly  /path/to/valgrind/nightly  tag

and supply conf/tag.conf and conf/tag.sendmail.

