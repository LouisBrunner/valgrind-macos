# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mcbreak
echo vgdb launched process attached\n
monitor v.set vgdb-error 999999
#
define checkstep
  set $old_pc=$pc
  step
  if $old_pc == $pc
    echo Bizarre the oldpc has not changed after step\n
    print $oldpc
    print $pc
  else
    echo old_pc has changed after step\n
  end
end
#
# break1 and break2
break t.c:112
break t.c:117
#
continue
# first break encountered.
checkstep
checkstep
checkstep
#
monitor v.set vgdb-error 0
#
next
print whoami("first")
print undefined
print i
checkstep
checkstep
next
print whoami("second")
print undefined
print i
next
print whoami("third")
print undefined
print i
next
print whoami("fourth")
print undefined
print i
# modify sleeps so as to have a shorter test:
print sleeps=1
#
print whoami("after next: inferior call pushed from mcbreak.stdinB.gdb")
continue
#
# encountered second break
step
finish
# delete all breaks
delete
continue
monitor v.info n_errs_found 
monitor v.info n_errs_found a
monitor v.info n_errs_found    b  
monitor v.info n_errs_found c d
monitor v.info n_errs_found eeeeeee    fffffff    ggggggg
# inferior call "in the middle" of an instruction is not working at least
# on all platforms, so comment the below.
# print whoami("after error: inferior call pushed from mcbreak.stdinB.gdb")
checkstep
monitor v.set vgdb-error 0
continue
# stop the process a.o. to avoid non deterministic output
monitor v.kill
quit
