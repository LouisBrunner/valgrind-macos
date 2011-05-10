# connect gdb to Valgrind gdbserver:
target remote | ./vgdb --wait=60 --vgdb-prefix=./vgdb-prefix-mcvabits
echo vgdb launched process attached\n
monitor vg.set vgdb-error 999999
#
#
# insert break:
break breakme
#
# continue till //1break:
continue
#
# up to main:
up
#
# print local string variables:
print main_name
print undefined
# save address of undefined 
set $0xundefined = &undefined
#
# Verif A-bits, V-bits, Get V-bits: A,V,G [0..9]
eval "monitor mc.check_memory addressable 0x%x 10", $0xundefined
eval "monitor mc.check_memory defined     0x%x 10", $0xundefined
eval "monitor mc.get_vbits                0x%x 10", $0xundefined
#
# continue till //2break:
continue
#
# A,V,G [0..9] after the undefinition of some bytes by executable:
eval "monitor mc.check_memory addressable 0x%x 10", $0xundefined
eval "monitor mc.check_memory defined     0x%x 10", $0xundefined
eval "monitor mc.get_vbits                0x%x 10", $0xundefined
#
# Redefine [2..4]
set $0xundefined_2 = (char*)$0xundefined + 2
eval "monitor mc.make_memory defined 0x%x 3", $0xundefined_2
# A,V,G
eval "monitor mc.check_memory addressable 0x%x 10", $0xundefined
eval "monitor mc.check_memory defined     0x%x 10", $0xundefined
eval "monitor mc.get_vbits                0x%x 10", $0xundefined
#
# Undefine [2..5]
eval "monitor mc.make_memory  undefined   0x%x 4", $0xundefined_2
# A,V,G [0..9]
eval "monitor mc.check_memory addressable 0x%x 10", $0xundefined
eval "monitor mc.check_memory defined     0x%x 10", $0xundefined
eval "monitor mc.get_vbits                0x%x 10", $0xundefined
#
# noaccess [2..3]
eval "monitor mc.make_memory  noaccess    0x%x 2", $0xundefined_2
# A,V,G [0..1]
eval "monitor mc.check_memory addressable 0x%x 2", $0xundefined
eval "monitor mc.check_memory defined     0x%x 2", $0xundefined
eval "monitor mc.get_vbits                0x%x 2", $0xundefined
# A,V,G [2..3]
eval "monitor mc.check_memory addressable 0x%x 2", $0xundefined_2
eval "monitor mc.check_memory defined     0x%x 2", $0xundefined_2
eval "monitor mc.get_vbits                0x%x 2", $0xundefined_2
# A,V,G [4..9]
set  $0xundefined_4 = (char*) $0xundefined_2 + 2
eval "monitor mc.check_memory addressable 0x%x 6", $0xundefined_4
eval "monitor mc.check_memory defined     0x%x 6", $0xundefined_4
eval "monitor mc.get_vbits                0x%x 6", $0xundefined_4
#
# ifaddressabledefined undefined[0..9]
eval "monitor mc.make_memory  ifaddressabledefined 0x%x 10", $0xundefined
# A,V,G
eval "monitor mc.check_memory addressable 0x%x 10", $0xundefined
eval "monitor mc.check_memory defined     0x%x 10", $0xundefined
eval "monitor mc.get_vbits                0x%x 10", $0xundefined
#
monitor vg.kill
quit
