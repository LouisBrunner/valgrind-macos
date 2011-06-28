# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mcinfcallRU
echo vgdb launched process attached\n
monitor v.set vgdb-error 999999
#
# We will interrupt in a few seconds (be sure the main task is ready).
# Once it is ready, we still have to wait to be sure it is running.
shell ./simulate_control_c --vgdb-prefix=./vgdb-prefix-mcinfcallRU 1 grep main mcinfcallRU.stderr.out
#
continue
info threads
thread apply all bt full
# Would like to call this for all threads with 'thread apply all', but due to unfair scheduling,
# the below can either take a long time and/or have threads finished
# before they have a chance to execute the whoami
# thread apply all
print whoami("inferior call pushed from gdb in mcinfcallRU.stdinB.gdb")
monitor v.kill
quit
