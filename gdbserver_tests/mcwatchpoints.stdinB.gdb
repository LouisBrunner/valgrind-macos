# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mcwatchpoints
echo vgdb launched process attached\n
monitor v.set vgdb-error 999999
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
rwatch undefined[9]
awatch undefined[9]
watch  undefined[9]
#
# now we should encounter 4 break points
continue
continue
continue
continue
del
break watchpoints.c:70
# continue till //break2:
continue
# trigger gdb reading data with big packets:
p *(k50)@50000
continue
quit
