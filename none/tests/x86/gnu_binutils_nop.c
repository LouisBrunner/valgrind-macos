int main(void)
{
    // GNU binutils uses various opcodes as alternatives for nop
    // the idea is that it is faster to execute one large opcode
    // with no side-effects than multiple repetitions of the
    // single byte 'nop'. This gives more choice when code
    // needs to be padded.
   
   // the following is based on
   // https://sourceware.org/cgit/binutils-gdb/tree/gas/config/tc-i386.c#n1256

    // one byte
    __asm__ __volatile__("nop");
    // two bytes
    __asm__ __volatile__("xchg %ax,%ax");
    // three bytes
    //__asm__ __volatile__("leal 0(%esi),%esi");
    __asm__ __volatile__(".byte 0x8d,0x76,0x00");
    // four bytes
    //__asm__ __volatile__("leal 0(%esi,%eiz),%esi");
    __asm__ __volatile__(".byte 0x8d,0x74,0x26,0x00");
    // five bytes
    //__asm__ __volatile__("leal %cs:0(%esi,%eiz),%esi");
    __asm__ __volatile__(".byte 0x2e,0x8d,0x74,0x26,0x00");
    // six bytes
    //__asm__ __volatile__("leal 0L(%esi),%esi");
    __asm__ __volatile__(".byte 0x8d,0xb6,0x00,0x00,0x00,0x00");
    // seven bytes
    //__asm__ __volatile__("leal 0L(%esi,%eiz),%esi");
    __asm__ __volatile__(".byte 0x8d,0xb4,0x26,0x00,0x00,0x00,0x00");
    // eight bytes
    //__asm__ __volatile__("leal %cs:0L(%esi,%eiz),%esi");
    __asm__ __volatile__(".byte 0x2e,0x8d,0xb4,0x26,0x00,0x00,0x00,0x00");
}
