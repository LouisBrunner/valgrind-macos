# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-nlself_invalidate
echo vgdb launched process attached\n
# place a breakpoint that will cause an invalidation of the currently executed translation
break *top
# Now, continue till the end. This should not crash
continue
continue
continue
continue
continue
continue
# and the process should exit very quickly now
quit
