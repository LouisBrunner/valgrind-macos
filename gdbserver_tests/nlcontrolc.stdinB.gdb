# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-nlcontrolc
echo vgdb launched process attached\n
monitor vg.set vgdb-error 999999
#
#
# simulate control-c in a few seconds
shell ./simulate_control_c --vgdb-prefix=./vgdb-prefix-nlcontrolc 10
#
continue
#
# Here, all tasks should be blocked in a loooonnnng select, all in WaitSys
info threads
# We will unblock them by changing their timeout argument
# To avoid going into the frame where the timeval arg is,
# it has been defined as global variables, as the nr
# of calls on the stack differs between 32bits and 64bits,
# and/or between OS.
# ensure select finishes in a few milliseconds max:
p t[0].tv_sec = 0
p t[1].tv_sec = 0
p t[2].tv_sec = 0
p t[3].tv_sec = 0
#
# We will change the burning parameters in a few  seconds
shell ./simulate_control_c --vgdb-prefix=./vgdb-prefix-nlcontrolc 10
#
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
