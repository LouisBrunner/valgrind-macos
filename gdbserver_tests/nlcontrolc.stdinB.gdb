# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-nlcontrolc
echo vgdb launched process attached\n
monitor v.set vgdb-error 999999
#
#
# simulate control-c in a few seconds
shell ./simulate_control_c --vgdb-prefix=./vgdb-prefix-nlcontrolc 1 grep main nlcontrolc.stderr.out
#
continue
#
# Threads are burning cpu now
# We would like to fully test info threads here, but which thread are Runnable
# or Yielding is unpredictable. With a recent enough gdb, check the nr of
# threads by state using pipe commands and grep/wc.
init-if-undefined $_gdb_major = 0
init-if-undefined $_gdb_minor = 0
if $_gdb_major >= 9
  | info threads | grep VgTs_Runnable | wc -l
  | info threads | grep VgTs_Yielding | wc -l
else
  echo 1\n
  echo 3\n
end
# We change the variables so that all the threads are blocked in a syscall
p burn = 0
p sleepms = 1000000
#
#
shell ./simulate_control_c --vgdb-prefix=./vgdb-prefix-nlcontrolc 1 grep changed nlcontrolc.stdoutB.out
#
echo changed burning parameters to sleeping parameters\n
continue
# Here, all tasks should be blocked in a loooonnnng select, all in WaitSys
info threads
# We reset the sleepms to 0. The threads should still be blocked in the syscall
p sleepms = 0
shell ./simulate_control_c --vgdb-prefix=./vgdb-prefix-nlcontrolc 1 grep reset nlcontrolc.stdoutB.out
#
echo reset to sleeping parameters\n
continue
# threads should still be blocked in a loooonnnng select, all in WaitSys
info threads
if $_gdb_major >= 9
  | info threads | grep VgTs_WaitSys | wc -l
else
  echo 4\n
end
# Make the process die.
kill
quit
