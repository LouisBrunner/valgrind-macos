# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-nlvgdbsigqueue
echo vgdb launched process attached\n
monitor v.set vgdb-error 999999
#
#
# simulate control-c 1 second after having seen sleepers program outputting 'main'
shell ./simulate_control_c --vgdb-prefix=./vgdb-prefix-nlvgdbsigqueue 1 grep main nlvgdbsigqueue.stderr.out
#
# send SIGUSR1/SIGUSR1 in a few seconds, after vgdb has attached
# vgdb will attach when it will receive the control-c
shell ./send_signal USR1 --vgdb-prefix=./vgdb-prefix-nlvgdbsigqueue 0 grep attachedwaitingforsigusr1 nlvgdbsigqueue.stdoutB.out
shell ./send_signal USR1 --vgdb-prefix=./vgdb-prefix-nlvgdbsigqueue 1 grep attachedwaitingforsigusr1 nlvgdbsigqueue.stdoutB.out
#
echo continuing to have vgdb interrupted by simulate_control_c\n
continue
#
# Now vgdb should have received the interrupt, and so has
# attached to the sleeping process.
# wait for the USR sig to be sent, that will be queued by vgdb.
echo attachedwaitingforsigusr1\n
# send_signal tries every second the guardcmd
# so we must now wait to be (somewhat) sure the 2 SIGUSR1 are emitted
shell sleep 4
# continue, so as to have vgdb sending queued signals when PTRACE_DETACHing
echo continuing to receive first SIGUSR1\n
continue
# simulate a control c to afterwards stop the execution
shell ./simulate_control_c --vgdb-prefix=./vgdb-prefix-nlvgdbsigqueue 1
echo continuing to receive second SIGUSR1\n
continue
kill
quit
