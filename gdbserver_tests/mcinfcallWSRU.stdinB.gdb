# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mcinfcallWSRU
echo vgdb launched process attached\n
monitor vg.set vgdb-error 999999
#
# ensure all threads are known
break sleeper_or_burner
continue
continue
continue
continue
#
# Here the 4 threads have been started.
# We will interrupt in a few seconds (be sure all tasks are in Runnable/Yielding state
# or in WaitSys state.
shell ./simulate_control_c --vgdb-prefix=./vgdb-prefix-mcinfcallWSRU 10
#
continue
#
thread 1
print whoami("thread 1 inferior call pushed from gdb in mcinfcallWSRU.stdinB.gdb")
thread 2
print whoami("thread 2 inferior call pushed from gdb in mcinfcallWSRU.stdinB.gdb")
thread 3
print whoami("thread 3 inferior call pushed from gdb in mcinfcallWSRU.stdinB.gdb")
thread 4
print whoami("thread 4 inferior call pushed from gdb in mcinfcallWSRU.stdinB.gdb")
monitor vg.kill
quit
