# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mcmain_pic
echo vgdb launched process attached\n
source mcmain_pic.heur
monitor v.set vgdb-error 999999
#
# break
break main_pic.c:12
#
continue
# first break encountered.
print another_func("called from gdb")
#
print &main
print &another_func
continue
quit
