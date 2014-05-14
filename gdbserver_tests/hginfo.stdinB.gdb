# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-hginfo
echo vgdb launched process attached\n
monitor v.set vgdb-error 999999
#
#
# insert break:
break breakme
#
# continue till each break and execute via gdb the monitor commands
# ptr must be allocated at this state:
continue
monitor info locks
eval "monitor v.info location %p", ptr
# ptr must be freed at this state
continue
monitor info locks
eval "monitor v.info location %p", ptr
continue
quit
