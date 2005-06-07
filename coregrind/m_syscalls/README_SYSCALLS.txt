
This module handles the complex business of handing system calls off
to the host and then fixing up the guest state accordingly.  It
interacts complicatedly with signals and to a less extent threads.

There are some important caveats regarding how to write the PRE and
POST wrappers for syscalls.  It is important to observe these, else
you will have to track down almost impossibly obscure bugs.  These
caveats are described in comments at the top of syscalls-main.c.

The main file is syscalls-main.c.  It contains all the driver logic
and a great deal of commentary.  The wrappers themselves live in
syscalls-generic.c, syscalls-${OS}.c and syscalls-${PLATFORM}.c.

