# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-nlvgdbsigqueue
echo vgdb launched process attached\n
monitor v.set vgdb-error 999999
#
#
# simulate control-c in a 1 second
shell ./simulate_control_c --vgdb-prefix=./vgdb-prefix-nlvgdbsigqueue 1 grep main nlvgdbsigqueue.stderr.out
#
# send SIGUSR1/SIGUSR1 in a few seconds, when vgdb is attached
shell ./send_signal USR1 --vgdb-prefix=./vgdb-prefix-nlvgdbsigqueue 4
shell ./send_signal USR1 --vgdb-prefix=./vgdb-prefix-nlvgdbsigqueue 4
#
echo continuing to have vgdb interrupted by simulate_control_c\n
continue
#
# Now vgdb should have received the interrupt, and so has
# attached to the sleeping process.
# wait for the USR sig to be sent, that will be queued by vgdb.
shell sleep 8
# continue, so as to have vgdb sending queued signals when PTRACE_DETACHing
echo continuing to receive first SIGUSR1\n
continue
# simulate a control c to afterwards stop the execution
shell ./simulate_control_c --vgdb-prefix=./vgdb-prefix-nlvgdbsigqueue 1
echo continuing to receive second SIGUSR1\n
continue
kill
quit
