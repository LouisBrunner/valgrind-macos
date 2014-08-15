# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-hgtls
echo vgdb launched process attached\n
monitor v.set vgdb-error 999999
#
#
# insert break:
break tls.c:55
command
set $tls_ip = main
if test == &tests[0]
  set $tls_ip = &race
end
if test == &tests[1]
  set $tls_ip = &local
end
if test == &tests[2]
  set $tls_ip = &global
end
if test == &tests[3]
  set $tls_ip = &static_extern
end
if test == &tests[4]
  set $tls_ip = &so_extern
end
if test == &tests[5]
  set $tls_ip = &so_local
end
if test == &tests[6]
  set $tls_ip = &global
end
printf "test %s tls_ip %p ip %p equal %d\n", test->name, $tls_ip, ip, $tls_ip == ip
continue
end
# continue till the end
continue
quit
