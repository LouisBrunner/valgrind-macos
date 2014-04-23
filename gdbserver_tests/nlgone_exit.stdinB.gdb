# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-nlgone-exit
echo vgdb launched process attached\n
# continue after startup
continue
# continue at the last instruction
continue
# see program is gone with exit code
quit
