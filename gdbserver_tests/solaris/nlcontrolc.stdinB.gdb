# connect gdb to Valgrind gdbserver:
target remote | ../vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-solaris-nlcontrolc
echo vgdb launched process attached\n
monitor v.set vgdb-error 999999
#
#
# simulate control-c in a few seconds
shell ../simulate_control_c --vgdb-prefix=./vgdb-prefix-solaris-nlcontrolc 1 grep main nlcontrolc.stderr.out
#
continue
#
# Here, all tasks should be blocked in a loooonnnng select, all in WaitSys
info threads
# After the timeout expires, threads will unblock.
#
# We will change the burning parameters in a few seconds
shell ../simulate_control_c --vgdb-prefix=./vgdb-prefix-solaris-nlcontrolc 6 grep CPU nlcontrolc.stdoutB.out
#
echo Now threads are burning CPU\n
continue
#
# Threads are burning cpu now
# We would like to test info threads here, but which thread are Runnable or Yielding
# is unpredictable.
# info threads
p burn = 0
p loops = 0
p report_finished = 0
continue
# and the process should stop very quickly now
quit
