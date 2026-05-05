// Reproducer for https://bugs.kde.org/show_bug.cgi?id=406674
//
// BACKGROUND
// ----------
// Valgrind's memcheck has a false positive triggered by the x86-64 sequence:
//
//     shlb  $0x4, %al     ; 8-bit left shift
//     je    <label>       ; branch on zero flag set by that shift
//
// Valgrind's guest_amd64_spechelper() has a fast-path optimisation for some
// shift+branch patterns (e.g. AMD64G_CC_OP_SHRL + CondZ) but is missing the
// equivalent rule for AMD64G_CC_OP_SHLB + CondZ.  Without it Valgrind falls
// back to a conservative "unknown" result and reports a spurious
// "Conditional jump or move depends on uninitialised value(s)" error even
// when the bitfield was fully initialised.
//
// The original testcase did not trigger the issue with GCC which does
// not generate the code pattern triggering the false positive.  Due to
// C integer-promotion rules, `uint8_t v; v << 4` is computed as a
// 32-bit operation.  GCC exploits this and emits:
//
//     movzbl %al, %eax      ; zero-extend to 32 bits
//     shll   $0x4, %eax     ; 32-bit SHL (CC_OP_SHLL, not SHLB)
//     testb  $-0x11, %al    ; separate TEST resets the CC
//     je     ...
//
// This version of the code uses the LLVM codegen pattern in
// inline asm to ensure that we get the right code.

#include <cstring>
#include <cstdint>

struct A {
    int  field : 4;   // occupies the low 4 bits of the first byte
    void *buf;
};

__attribute__((noinline))
void init(A *a) {
    a->field = 0;
    a->buf   = nullptr;
}

int main() {
    A a;
    init(&a);

    // Read the raw byte that contains the bitfield.
    uint8_t raw;
    std::memcpy(&raw, reinterpret_cast<const char *>(&a), 1);

    // Use inline asm to emit exactly:
    //     shlb $4, <reg>      ; shift 4-bit field into high nibble
    //     je   <zero_label>   ; branch if field was zero
    //
    // This bypasses C integer-promotion (which would widen to 32 bits and
    // let GCC insert an extra TEST instruction that resets the flags), so
    // both GCC and clang emit the shlb+je sequence that Valgrind
    // mishandled.  The variable `skip` is set to 1 when field==0.
    int skip;
    asm volatile(
        "shlb $4, %b1\n\t"   // 8-bit SHL: sets ZF if result is 0 (field==0)
        "je   1f\n\t"        // jump if field was 0  <-- Valgrind false pos.
        "movl $0, %0\n\t"    // field != 0
        "jmp  2f\n\t"
        "1: movl $1, %0\n\t" // field == 0
        "2:"
        : "=r"(skip), "+q"(raw)   // +q: must be a-b-c-d byte-addressable reg
        :
        : "cc"
    );

    if (!skip) {
        // field was non-zero; raw now holds (original_raw << 4) in low byte
        if (a.buf) {
            if (raw != 0x10 && raw != 0x20) {   // field != 1 and field != 2
                asm volatile("" ::: "memory");
            }
            a.buf = nullptr;
        }
    }

    return 0;
}
