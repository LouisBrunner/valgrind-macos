# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mcblocklistsearch
echo vgdb launched process attached\n
monitor v.set vgdb-error 999999
#
#
# insert break after the allocation of A
break leak-tree.c:42
# insert break after returning from function f
break leak-tree.c:68
#
# continue till //1break:
continue
# save the value of A
set $0xA = t
#
# continue till 2nd break
continue
#
# check who points at A
eval "monitor who_points_at 0x%lx 1", $0xA
# do a leak check, and then list the blocks lost
echo full leak search \n
monitor leak_check full reachable any
#
echo block list 6 D \n
monitor block_list 6
echo block list 7 C F G \n
monitor block_list 7
monitor v.kill
quit
