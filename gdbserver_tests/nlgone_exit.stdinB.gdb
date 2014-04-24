# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-nlgone-exit
echo vgdb launched process attached\n
# continue after startup
echo filter_gdb BEGIN drop\n
continue
echo filter_gdb END drop\n
# continue at the last instruction
continue
# see program is gone with exit code
quit
