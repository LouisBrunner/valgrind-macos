# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mcsignopass
echo vgdb launched process attached\n
monitor vg.set vgdb-error 999999
#
# instruct gdb to not pass (i.e. ignore) these signals.
#
# Trap the below signals, we make them stop and then continue.
# For SIGSEGV, we make it continue a few times, till we pass it.
handle SIGSEGV nopass print stop
handle SIGBUS  pass print stop
handle SIGFPE  pass print stop
#
continue
#
# SIGTRAP : caused by invalid write error detected by memcheck
continue
#
# SIGSEGV can't be ignored, so it is re-signaled. We continue many times
# to be sure it is this signal which is re-signalled. Then will pass it.
continue
continue
continue
continue
continue
continue
continue
continue
continue
#
# Change handling so that we just see the 2nd SIGSEGV
handle SIGSEGV pass print nostop
continue
#
# SIGBUS will be shown and passed:
continue
#
# then SIGFPE is shown and passed:
continue
#
# program will exit
quit
