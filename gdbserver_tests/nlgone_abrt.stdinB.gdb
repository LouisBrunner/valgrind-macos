# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-nlgone-abrt
echo vgdb launched process attached\n
continue
# see process get a fatal signal
continue
# see program is gone
quit
