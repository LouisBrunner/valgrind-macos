# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mcleak
echo vgdb launched process attached\n
monitor vg.set vgdb-error 999999
#
#
# insert break:
break breakme
#
# continue till each break and execute via gdb the leak search as done in the C code.
continue
#
#
#   fprintf(stderr, "expecting details 10 bytes reachable\n"); fflush(stderr); breakme();
up
monitor mc.leak_check any reachable full
continue
#   VALGRIND_DO_LEAK_CHECK;
#
#   fprintf(stderr, "expecting to have NO details\n"); fflush(stderr);
up
monitor mc.leak_check increased reachable full
continue
#   VALGRIND_DO_ADDED_LEAK_CHECK;
#
#   b10--; // lose b10
#   b21 = malloc (21);
#   fprintf(stderr, "expecting details +10 bytes lost, +21 bytes reachable\n"); fflush(stderr); breakme();
up
monitor mc.leak_check increased reachable full
continue
#   VALGRIND_DO_ADDED_LEAK_CHECK;
#
#   for (i = 0; i < 2; i ++)
#      b32_33[i] = malloc (32+i);
#   fprintf(stderr, "expecting details +65 bytes reachable\n"); fflush(stderr); breakme();
up
monitor mc.leak_check increased reachable full
continue
#   VALGRIND_DO_ADDED_LEAK_CHECK;
#
#   fprintf(stderr, "expecting to have NO details\n"); fflush(stderr); breakme();
up
monitor mc.leak_check increased reachable full
continue
#   VALGRIND_DO_ADDED_LEAK_CHECK;
#
#   b10++;
#   fprintf(stderr, "expecting details +10 bytes reachable\n"); fflush(stderr); breakme();
up
monitor mc.leak_check increased reachable full
continue
#   VALGRIND_DO_ADDED_LEAK_CHECK;
#
#   b10--;
#   fprintf(stderr, "expecting details -10 bytes reachable, +10 bytes lost\n"); fflush(stderr); breakme();
up
monitor mc.leak_check changed reachable full
continue
#   VALGRIND_DO_CHANGED_LEAK_CHECK;
#
#   b10++;
#   fprintf(stderr, "expecting details -10 bytes lost, +10 bytes reachable\n"); fflush(stderr); breakme();
up
monitor mc.leak_check changed reachable full
continue
#   VALGRIND_DO_CHANGED_LEAK_CHECK;
#
#   b32_33[0]--;
#   fprintf(stderr, "expecting details 32 (+32) bytes lost, 33 (-32) bytes reachable\n"); fflush(stderr); breakme();
up
monitor mc.leak_check changed reachable full
continue
#   VALGRIND_DO_CHANGED_LEAK_CHECK;
#
quit
