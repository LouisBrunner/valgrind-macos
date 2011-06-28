# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mssnapshot
echo vgdb launched process attached\n
monitor v.set vgdb-error 999999
#
#
# insert break:
break main
#
# continue till main
continue
#
# test the massif help
monitor help
#
# test non detailed and detailed snapshot
monitor snapshot
monitor detailed_snapshot
#
#
monitor v.kill
quit
