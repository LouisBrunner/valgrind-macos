# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mcmain_pic
echo vgdb launched process attached\n
monitor vg.set vgdb-error 999999
#
# break
break main
#
continue
# first break encountered.
print another_func("called from gdb")
#
print &main
print &another_func
continue
quit
