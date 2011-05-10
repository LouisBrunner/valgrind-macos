# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mcclean_after_fork
echo vgdb launched process attached\n
monitor vg.set vgdb-error 999999
#
# put a break in main, and then a watch
# also put breaks in code that only the child will execute.
# These breaks should not be encountered.
break clean_after_fork.c:9
break clean_after_fork.c:18
break clean_after_fork.c:20
#
continue
# first break encountered.
# put a read watchpoint on mem
# we expect that the read watchpoint is not triggered in the child
# (as we expect it will be cleared at fork).
rwatch mem
#
continue
#
# we should now have encountered the read watchpoint in the parent.
# let's kill the parent:
monitor vg.kill
quit
