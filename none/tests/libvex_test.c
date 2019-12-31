#include <stdio.h>
#include <stdlib.h>
# if defined(VGO_linux)
#include <endian.h>
# elif defined(VGO_darwin)
#include <machine/endian.h>
# endif
#include "../../VEX/pub/libvex.h"

Bool return_false(void*cb, Addr ad)
{
   return False;
}
UInt return_0(void *cb, VexRegisterUpdates* pxControl,
              const VexGuestExtents *vge)
{
   return 0;
}

__attribute__ ((noreturn))
static void failure_exit()
{
   fflush(stdout);
   fprintf(stderr, "//// failure exit called by libVEX\n");
   exit(1);
}

__attribute__ ((noreturn))
static void failure_dispcalled()
{
   fflush(stdout);
   fprintf(stderr, "//// unexpected call to a disp function by libVEX\n");
   exit(1);
}

static void log_bytes (const HChar* chars, SizeT nbytes )
{
   printf("%*s", (int)nbytes, chars);
}

// Returns the endness of the system we are running on.
// We use that as the endness of arch that supports both
// little and big endian.
static VexEndness running_endness (void)
{
#if __BYTE_ORDER == __LITTLE_ENDIAN
   return VexEndnessLE;
#elif __BYTE_ORDER == __BIG_ENDIAN
   return VexEndnessBE;
#else
   fprintf(stderr, "cannot determine endianness\n");
   exit(1);
#endif
}

// noinline, as this function is also the one we decode.
__attribute__((noinline)) static void get_guest_arch(VexArch    *ga)
{
#if defined(VGA_x86)
   *ga = VexArchX86;
#elif defined(VGA_amd64)
   *ga = VexArchAMD64;
#elif defined(VGA_arm)
   *ga = VexArchARM;
#elif defined(VGA_arm64)
   *ga = VexArchARM64;
#elif defined(VGA_ppc32)
   *ga = VexArchPPC32;
#elif defined(VGA_ppc64be) || defined(VGA_ppc64le) 
   *ga = VexArchPPC64;
#elif defined(VGA_s390x)
   *ga = VexArchS390X;
#elif defined(VGA_mips32)
   *ga = VexArchMIPS32;
#elif defined(VGA_mips64)
   *ga = VexArchMIPS64;
#elif defined(VGA_nanomips)
   *ga = VexArchNANOMIPS;
#else
   missing arch;
#endif
}

static VexEndness arch_endness (VexArch va) {
   switch (va) {
   case VexArch_INVALID: failure_exit();
   case VexArchX86:    return VexEndnessLE;
   case VexArchAMD64:  return VexEndnessLE;
   case VexArchARM:    return VexEndnessLE;
   case VexArchARM64:  return VexEndnessLE;
   case VexArchPPC32:  return VexEndnessBE;
   case VexArchPPC64:
      /* ppc64 supports BE or LE at run time. So, on a LE system,
         returns LE, on a BE system, return BE. */
      return running_endness();
   case VexArchS390X:  return VexEndnessBE;
   case VexArchMIPS32:
   case VexArchMIPS64:
   case VexArchNANOMIPS:
      /* mips32/64 supports BE or LE, but at compile time.
         If mips64 is compiled on a non mips system, the VEX lib
         is missing bit and pieces of code related to endianness.
         The mandatory code for this test is then compiled as BE.
         So, if this test runs on a mips system, returns the
         running endianness. Otherwise, returns BE as this one
         has the more chances to work. */
      {
         VexArch ga;
         get_guest_arch( &ga);

         if (ga == VexArchMIPS64 || ga == VexArchMIPS32 || ga == VexArchNANOMIPS)
            return running_endness();
         else
            return VexEndnessBE;
      }
   default: failure_exit();
   }
}

/* returns whatever kind of hwcaps needed to make
   the host and/or guest VexArch happy. */
static UInt arch_hwcaps (VexArch va) {
   switch (va) {
   case VexArch_INVALID: failure_exit();
   case VexArchX86:    return 0;
   case VexArchAMD64:  return 0;
   case VexArchARM:    return 7;
   case VexArchARM64:  return 0;
   case VexArchPPC32:  return 0;
   case VexArchPPC64:  return 0;
   case VexArchS390X:  return VEX_HWCAPS_S390X_LDISP;
#if (__mips_isa_rev>=6)
   case VexArchMIPS32: return VEX_PRID_COMP_MIPS | VEX_MIPS_CPU_ISA_M32R6 |
                              VEX_MIPS_HOST_FR;
   case VexArchMIPS64: return VEX_PRID_COMP_MIPS | VEX_MIPS_CPU_ISA_M64R6 |
                              VEX_MIPS_HOST_FR;
#else
   case VexArchMIPS32: return VEX_PRID_COMP_MIPS;
   case VexArchMIPS64: return VEX_PRID_COMP_MIPS | VEX_MIPS_HOST_FR;
#endif
   case VexArchNANOMIPS: return 0;
   default: failure_exit();
   }
}

static Bool mode64 (VexArch va) {
   switch (va) {
   case VexArch_INVALID: failure_exit();
   case VexArchX86:    return False;
   case VexArchAMD64:  return True;
   case VexArchARM:    return False;
   case VexArchARM64:  return True;
   case VexArchPPC32:  return False;
   case VexArchPPC64:  return True;
   case VexArchS390X:  return True;
   case VexArchMIPS32: return False;
   case VexArchMIPS64: return True;
   case VexArchNANOMIPS: return False;
   default: failure_exit();
   }
}

static void show_vta(char *msg, VexTranslateArgs *vta)
{
   printf("//// %s translating guest %s(%d) %s %dbits to host %s(%d)"
          " %s %dbits\n",
          msg,
          LibVEX_ppVexArch(vta->arch_guest),
          vta->arch_guest,
          LibVEX_ppVexEndness(arch_endness(vta->arch_guest)),
          mode64(vta->arch_guest) ? 64 : 32,
          LibVEX_ppVexArch(vta->arch_host),
          vta->arch_host,
          LibVEX_ppVexEndness(arch_endness(vta->arch_host)),
          mode64(vta->arch_host) ? 64 : 32);
}


int main(int argc, char **argv)
{
   const int multiarch = argc > 1 ? atoi(argv[1]) : 0;
   // 0 means: do not do multiarch
   // > 0 means: do multiarch
   // > VexArch_INVALID means: do multiarch, only and specifically
   // with the host arch  equal to multiarch
   // (ugly interface, but hey, that is for testing only special cases only).
   const int endness_may_differ = argc > 2 ? atoi(argv[2]) : 0;
   const int wordsize_may_differ = argc > 3 ? atoi(argv[3]) : 0;
   // Note: if multiarch > VexArch_INVALID, then endness_may_differ
   // and wordsize_may_differ are ignored.

   // So, here are examples of usage:
   //  * run only host == guest:
   //     ./libvexmultiarch_test
   //     ./libvex_test
   //  * run all combinations (this will abort very soon :):
   //     ./libvexmultiarch_test 1 1 1
   //  * run all combinations that are supposed to  work by default :
   //     ./libvexmultiarch_test 1 0 0
   //  * run a specific host arch (e.g. 1028 i.e. VexArchARM64)
   //     ./libvexmultiarch_test 1028
   //  * show how a single arch VEX lib reports its failure when host != guest
   //     ./libvex_test 1 0 0
   

   VexArch guest_arch;
   VexEndness guest_endness;

   VexControl vcon;

   VexGuestExtents  vge;
   VexTranslateArgs vta;
   VexTranslateResult vtr;

   UChar host_bytes[10000];
   Int   host_bytes_used;

   LibVEX_default_VexControl(&vcon);
   LibVEX_Init (failure_exit, log_bytes, 3, &vcon);

   get_guest_arch (&guest_arch);
   guest_endness = arch_endness (guest_arch);
   
   LibVEX_default_VexArchInfo(&vta.archinfo_guest);
   LibVEX_default_VexArchInfo(&vta.archinfo_host);
   LibVEX_default_VexAbiInfo (&vta.abiinfo_both);

   // Use some values that makes AMD64 happy.
   vta.abiinfo_both.guest_stack_redzone_size = 128;

   // Use some values that makes ARM64 happy.
   vta.archinfo_guest.arm64_dMinLine_lg2_szB = 6;
   vta.archinfo_guest.arm64_iMinLine_lg2_szB = 6;

   // Prepare first for a translation where guest == host
   // We will translate the get_guest_arch function
   vta.arch_guest                 = guest_arch;
   vta.archinfo_guest.endness     = guest_endness;
   vta.archinfo_guest.hwcaps      = arch_hwcaps (vta.arch_guest);
   vta.arch_host                  = guest_arch;
   vta.archinfo_host.endness      = guest_endness;
   vta.archinfo_host.hwcaps       = arch_hwcaps (vta.arch_host);
   vta.callback_opaque            = NULL;
   vta.guest_bytes                = (UChar*) get_guest_arch;
   vta.guest_bytes_addr           = (Addr) get_guest_arch;
   vta.chase_into_ok              = return_false;
   vta.guest_extents              = &vge;
   vta.host_bytes                 = host_bytes;
   vta.host_bytes_size            = sizeof host_bytes;
   vta.host_bytes_used            = &host_bytes_used;
   vta.instrument1                = NULL;
   vta.instrument2                = NULL;
   vta.finaltidy                  = NULL;
   vta.needs_self_check           = return_0;
   vta.preamble_function          = NULL;
   vta.traceflags                 = 0xFFFFFFFF;
   vta.sigill_diag                = False;
   vta.addProfInc                 = False;
   vta.disp_cp_chain_me_to_slowEP = failure_dispcalled;
   vta.disp_cp_chain_me_to_fastEP = failure_dispcalled;
   vta.disp_cp_xindir             = failure_dispcalled;
   vta.disp_cp_xassisted          = failure_dispcalled;

   
   show_vta("host == guest", &vta);
   vtr = LibVEX_Translate ( &vta );
   if (vtr.status != VexTransOK) 
      return 1;

   // Now, try various combinations, if told to do so:
   //   host            != guest, 
   //   endness(host)   != endness(guest)     (not well supported)
   //   wordsize (host) != wordsize (guest)   (not well supported)
   // The not well supported combinations are not run, unless requested
   // explicitly via command line arguments.
   if (multiarch) {
      VexArch va;
      for (va = VexArchX86; va <= VexArchNANOMIPS; va++) {
         vta.arch_host = va;
         vta.archinfo_host.endness = arch_endness (vta.arch_host);
         vta.archinfo_host.hwcaps = arch_hwcaps (vta.arch_host);
         if (arch_endness(va) != arch_endness(guest_arch) 
             && !endness_may_differ
             && multiarch != va) {
            show_vta("skipped (endness differs)", &vta);
            continue;
         }
         if (mode64(va) != mode64(guest_arch) 
             && !wordsize_may_differ
             && multiarch != va) {
            show_vta("skipped (word size differs)", &vta);
            continue;
         }
         if (multiarch > VexArch_INVALID
             && multiarch != va) {
            show_vta("skipped (!= specific requested arch)", &vta);
            continue;
         }
         show_vta ("doing", &vta);
         vtr = LibVEX_Translate ( &vta );
         if (vtr.status != VexTransOK) 
            return 1;
      }
   }

   printf ("//// libvex testing normal exit\n");
   return 0;
}
