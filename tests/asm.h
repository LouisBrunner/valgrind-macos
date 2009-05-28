// Header to factor out platform differences in asm code.

// On Darwin, all symbols get an underscore prepended when compiled.  If we
// use any such symbols in asm code, we need to add that underscore.  So in
// general, any symbol named in asm code should be wrapped by VG_SYM.

// This one is for use in inline asm in C files.
#if defined(VGO_darwin)
#define VG_SYM(x) "_"#x
#else
#define VG_SYM(x) #x
#endif

// This one is for use in asm files.
#if defined(VGO_darwin)
#define VG_SYM_ASM(x) _##x
#else
#define VG_SYM_ASM(x) x
#endif
