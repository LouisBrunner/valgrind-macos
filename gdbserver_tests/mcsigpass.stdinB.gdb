# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mcsigpass
echo vgdb launched process attached\n
monitor vg.set vgdb-error 999999
#
# After this continue, we will receive 5 signals.
continue
#
# SIGTRAP : caused by invalid write error detected by memcheck
continue
#
# SIGSEGV : line 99
continue
#
# SIGSEGV : line 104
continue
#
# SIGBUS  : line 109
continue
#
# SIGFPE  : line 114
continue
#
# program will exit
quit

