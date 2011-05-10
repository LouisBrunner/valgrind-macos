# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mcwatchpoints
echo vgdb launched process attached\n
monitor vg.set vgdb-error 999999
#
#
# insert break:
break breakme
#
# continue till //break1:
continue
#
# insert the watchpoints
rwatch undefined[0]
awatch undefined[4]
watch  undefined[8]
#
# now we should encounter 4 break points
continue
continue
continue
continue
del
continue
quit
