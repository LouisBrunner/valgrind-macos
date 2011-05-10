# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mcinfcallRU
echo vgdb launched process attached\n
monitor vg.set vgdb-error 999999
#
# We will interrupt in a few seconds (be sure all tasks are in
# Runnable/Yielding state). We need to wait enough seconds to be sure
# Valgrind has started to execute the threads.
# On a heavily loaded slow arm gcc compile farm system, 5 seconds
# was not enough.
shell ./simulate_control_c --vgdb-prefix=./vgdb-prefix-mcinfcallRU 10
#
continue
info threads
thread apply all bt full
# Would like to call this for all threads with 'thread apply all', but due to unfair scheduling,
# the below can either take a long time and/or have threads finished
# before they have a chance to execute the whoami
# thread apply all
print whoami("inferior call pushed from gdb in mcinfcallRU.stdinB.gdb")
monitor vg.kill
quit
