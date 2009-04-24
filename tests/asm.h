// Header to factor out platform differences in asm code.

// On Darwin, all symbols get an underscore prepended when compiled.  If we
// use any such symbols in asm code, we need to add that underscore.  So in
// general, any symbol named in asm code should be wrapped by VG_SYM.

// This one is for use in inline asm in C files.
#define VG_SYM(x) #x

// This one is for use in asm files.
#define VG_SYM_ASM(x) _#x
