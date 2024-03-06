#!/bin/sh

${CC} -c -x c -o dummylib.o - <<EOF
__attribute__((constructor))
static void mysystem_init(void) {
}
EOF

${CC} -c -x assembler -o dummydata.o - <<EOF
.section __DATA,__fake
.globl __DATA.__fake
__DATA.__fake:
dyld4_ProgramVars_NXArgcPtr:
  .quad 0
dyld4_ProgramVars_NXArgvPtr:
  .quad 0
dyld4_ProgramVars_environPtr:
  .quad 0
dyld4_ProgramVars___prognamePtr:
  .quad 0

.section __DATA,__dyld4
.globl __DATA.__dyld4
__DATA.__dyld4:
; APIs*               apis;
.quad 0 ; (set by dyld, null)
; void*               allImageInfos;
.quad 0 ; (set by dyld, null)
; dyld4::ProgramVars  defaultVars; (5 pointers)
;; const void*      mh;
.quad 0 ; (set by dyld, null)
;; int*             NXArgcPtr;
.quad dyld4_ProgramVars_NXArgcPtr ; (used by dyld, non-null)
;; const char***    NXArgvPtr;
.quad dyld4_ProgramVars_NXArgvPtr ; (used by dyld, non-null)
;; const char***    environPtr;
.quad dyld4_ProgramVars_environPtr ; (used by dyld, non-null)
;; const char**     __prognamePtr;
.quad dyld4_ProgramVars___prognamePtr ; (used by dyld, non-null)
; dyld3::DyldLookFunc dyldLookupFuncAddr;
.quad 0 ; (func pointer, used by dyld, non-null)
; void* (*tlv_get_addrAddr)(dyld3::MachOAnalyzer::TLV_Thunk*);
.quad 0 ; (used by dyld, non-null)
EOF

FLAGS="-dylib dummylib.o -L/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX14.2.sdk/usr/lib -lSystem"

/usr/bin/ld ${FLAGS} -o libmySystem.dylib -install_name /usr/lib/libSystem.B.dylib
/usr/bin/ld ${FLAGS} dummydata.o -o libmydyld.dylib -install_name /usr/lib/system/libdyld.dylib

/bin/rm -f dummylib.o dummydata.o

/usr/bin/python3 - libmySystem.dylib libmydyld.dylib <<EOF
#!/usr/bin/env python3
import os
import subprocess
import sys
import lief # TODO: not standard!

def main():
  for bin in sys.argv[1:]:
    bin_fixed = f"{bin}.fixed"
    binbin = lief.parse(bin)

    # Remove signature if present as it will be invalidated by our modifications
    binbin.remove_signature()
    # The whole point of those libraries is to avoid using the system ones,
    # so "cache cette bibliotheque que je ne saurais voir"
    binbin.remove(lief._lief.MachO.LOAD_COMMAND_TYPES.LOAD_DYLIB) # libSystem.B.dylib

    # Commit the changes and make the binary executable
    binbin.write(bin_fixed)
    os.replace(bin_fixed, bin)
    os.chmod(bin, 0o755)

    # Now we need to resign the binary
    subprocess.run(["codesign", "-f", "-s", "-", bin], check=True)

if __name__ == "__main__":
  main()
EOF
