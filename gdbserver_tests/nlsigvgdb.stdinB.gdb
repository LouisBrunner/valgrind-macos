# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-nlsigvgdb
echo vgdb launched process attached\n
monitor v.set vgdb-error 999999
#
#
# simulate control-c in a few seconds
# The control-c will cause a character to be sent to gdbserver, causing
# an invocation while the gdbserver is already busy.
shell ./simulate_control_c --vgdb-prefix=./vgdb-prefix-nlsigvgdb 1 grep continuing nlsigvgdb.stderrB.out
#
monitor v.wait 5000
#
# kill the process now
monitor v.kill
quit

