# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mssnapshot
echo vgdb launched process attached\n
monitor vg.set vgdb-error 999999
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
monitor ms.snapshot
monitor ms.detailed_snapshot
#
#
monitor vg.kill
quit
