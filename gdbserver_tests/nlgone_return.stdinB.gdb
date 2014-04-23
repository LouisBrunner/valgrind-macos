# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-nlgone-return
echo vgdb launched process attached\n
continue
# see program is gone
quit
