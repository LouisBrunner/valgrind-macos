
/* HOW TO USE

Compile test file (eg test_hello.c) to a .o

It must have an entry point called "entry" immediately preceded by the
bytes 0x11 0x22 0x33 0x44 0x55 0x66 0x77 0x88, so the entry point can
be found.

It must have ***NO RELOCATIONS*** -  objdump -r file.o shows none

*/

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "../pub/libvex_basictypes.h"
#include "../pub/libvex_guest_x86.h"
#include "../pub/libvex_guest_amd64.h"
#include "../pub/libvex.h"


static Int n_bbs_done = 0;

/* For providing services. */
static HWord serviceFn ( HWord arg1, HWord arg2 )
{
   switch (arg1) {
      case 0: /* EXIT */
         printf("---STOP---\n");
         printf("serviceFn:EXIT\n");
	 printf("%d bbs simulated\n", n_bbs_done);
         exit(0);
      case 1: /* PUTC */
         putchar(arg2);
         return 0;
      case 2: /* MALLOC */
         return (HWord)malloc(arg2);
      default:
         assert(0);
   }
}


#if defined(__i386__)
#  define VexGuestState             VexGuestX86State
#  define LibVEX_Guest_initialise   LibVEX_GuestX86_initialise
#  define VexArch                   VexArchX86
#  define VexSubArch                VexSubArchX86_sse1
#  define GuestPC                   guest_EIP
#else
#   error "Unknown arch"
#endif

#define TEST_FLAGS (1<<7) //|(1<<0)


/* guest state */
UInt gstack[50000];
VexGuestState gst;
VexControl vcon;

/* only used for the switchback transition */
/* i386: helper1 = &gst, helper2 = %EFLAGS */
HWord sb_helper1 = 0;
HWord sb_helper2 = 0;

/* translation cache */
#define N_TRANS_CACHE 100000
#define N_TRANS_TABLE 1000

ULong trans_cache[N_TRANS_CACHE];
VexGuestExtents trans_table [N_TRANS_TABLE];
ULong*          trans_tableP[N_TRANS_TABLE];

Int trans_cache_used = 0;
Int trans_table_used = 0;

static Bool chase_into_not_ok ( Addr64 dst ) { return False; }

/* -------------------- */
#if defined(__i386__)

/* continue execution on the real CPU (never returns) */
extern void switchback_asm(void);
asm(
"switchback_asm:\n"
"   movl sb_helper1, %eax\n"  // eax = guest state ptr
"   movl  16(%eax), %esp\n"   // switch stacks
"   pushl 56(%eax)\n"         // push continuation addr
"   movl sb_helper2, %ebx\n"  // get eflags
"   pushl %ebx\n"             // eflags:CA
"   pushl 0(%eax)\n"          //  EAX:eflags:CA
"   movl 4(%eax), %ecx\n" 
"   movl 8(%eax), %edx\n" 
"   movl 12(%eax), %ebx\n" 
"   movl 20(%eax), %ebp\n"
"   movl 24(%eax), %esi\n"
"   movl 28(%eax), %edi\n"
"   popl %eax\n"
"   popfl\n"
"   ret\n"
);
void switchback ( void )
{
   sb_helper1 = (HWord)&gst;
   sb_helper2 = LibVEX_GuestX86_get_eflags(&gst);
   switchback_asm(); // never returns
}

#else
#   error "Unknown arch (switchback)"
#endif

/* -------------------- */
static HWord f, gp, res;
extern void run_translation_asm(void);
asm(
"run_translation_asm:\n"
"   pushal\n"
"   movl gp, %ebp\n"
"   movl f, %eax\n"
"   call *%eax\n"
"   movl %eax, res\n"
"   popal\n"
"   ret\n"
);

void run_translation ( HWord translation )
{
   if (0)
      printf(" run translation %p\n", (void*)translation );
   f = translation;
   gp = (HWord)&gst;
   run_translation_asm();
   gst.GuestPC = res;
   n_bbs_done ++;
}

HWord find_translation ( Addr64 guest_addr )
{
   Int i;
   HWord res;
   if (0)
      printf("find translation %p ... ", ULong_to_Ptr(guest_addr));
   for (i = 0; i < trans_table_used; i++)
     if (trans_table[i].base[0] == guest_addr)
        break;
   if (i == trans_table_used) {
      if (0) printf("none\n");
      return 0; /* not found */
   }
   res = (HWord)trans_tableP[i];
   if (0) printf("%p\n", (void*)res);
   return res;
}

#define N_TRANSBUF 5000
static UChar transbuf[N_TRANSBUF];
void make_translation ( Addr64 guest_addr, Bool verbose )
{
   VexTranslateResult tres;
   Int trans_used, i, ws_needed;
   assert(trans_table_used < N_TRANS_TABLE);
   if (0)
      printf("make translation %p\n", ULong_to_Ptr(guest_addr));
   tres
      = LibVEX_Translate ( 
           VexArch, VexSubArch,
           VexArch, VexSubArch,
           ULong_to_Ptr(guest_addr), guest_addr,
           chase_into_not_ok,
           &trans_table[trans_table_used],
           transbuf, N_TRANSBUF, &trans_used,
           NULL,          /* instrument1 */
           NULL,          /* instrument2 */
           False,         /* cleanup after instrument */
           NULL, /* access checker */
           verbose ? TEST_FLAGS : 0
        );
   assert(tres == VexTransOK);
   ws_needed = (trans_used+7) / 8;
   assert(ws_needed > 0);
   assert(trans_cache_used + ws_needed < N_TRANS_CACHE);

   for (i = 0; i < trans_used; i++) {
     HChar* dst = ((HChar*)(&trans_cache[trans_cache_used])) + i;
     HChar* src = (HChar*)(&transbuf[i]);
     *dst = *src;
   }

   trans_tableP[trans_table_used] = &trans_cache[trans_cache_used];
   trans_table_used++;
   trans_cache_used += ws_needed;
}


static Int    stopAfter = 0;
static UChar* image     = NULL;
static Int    imageSz   = 0;
static UChar* entry     = NULL;


__attribute__ ((noreturn))
static
void failure_exit ( void )
{
   fprintf(stdout, "VEX did failure_exit.  Bye.\n");
   exit(1);
}

static
void log_bytes ( HChar* bytes, Int nbytes )
{
   fwrite ( bytes, 1, nbytes, stdout );
}


/* run simulated code forever (it will exit by calling
   serviceFn(0)). */
static void run_simulator ( void )
{
   static Addr64 last_guest = 0;
   Addr64 next_guest;
   HWord next_host;
   while (1) {
      if (n_bbs_done == stopAfter) {
         printf("---begin SWITCHBACK at %d---\n", n_bbs_done);
	 if (last_guest)
            make_translation(last_guest,True);
         printf("---  end SWITCHBACK at %d---\n", n_bbs_done);
         switchback();
         assert(0); /*NOTREACHED*/
      }

      next_guest = gst.GuestPC;

      if (next_guest == Ptr_to_ULong(&serviceFn)) {
         /* "do" the function call to serviceFn */
#        if defined(__i386__)
         {
            HWord esp = gst.guest_ESP;
            gst.guest_EIP = *(UInt*)(esp+0);
            gst.guest_EAX = serviceFn( *(UInt*)(esp+4), *(UInt*)(esp+8) );
            gst.guest_ESP = esp+4;
            next_guest = gst.guest_EIP;
         }
#        else
#        error "Unknown arch"
#        endif
      }

      next_host = find_translation(next_guest);
      if (next_host == 0) {
         make_translation(next_guest,False);
         next_host = find_translation(next_guest);
	 assert(next_host != 0);
      }
      last_guest = next_guest;
      run_translation(next_host);
   }
}


static void usage ( void )
{
   printf("usage: switchback file.o #bbs\n");
   exit(1);
}

int main ( Int argc, HChar** argv )
{
   HChar* oname;
   FILE* f;
   Int n, i;

   struct stat buf;

   if (argc != 3) 
      usage();

   oname = argv[1];
   stopAfter = atoi(argv[2]);

   if (stat(oname, &buf)) {
      printf("switchback: can't stat %s\n", oname);
      return 1;
   }

   imageSz = (Int)buf.st_size;
   printf("stopAfter = %d\n", stopAfter);
   printf("file size is %d\n", imageSz);

   /* Get the file aboard and find the entry point */
   image = malloc(imageSz);
   assert(image);

   f = fopen(oname, "r");
   assert(f);
   n = fread(image, 1, imageSz, f);
   assert(n == imageSz);
   fclose(f);

   assert(imageSz > 12);
   entry = NULL;
   for (n = 0; n < imageSz - 8; n++) {
     if (image[n+0] == 0x11 
         && image[n+1] == 0x22
         && image[n+2] == 0x33
         && image[n+3] == 0x44
         && image[n+4] == 0x55
         && image[n+5] == 0x66
         && image[n+6] == 0x77
         && image[n+7] == 0x88
	 ) {
       entry = &image[n+8];
       break;
     }
   }

   if (!entry) {
      printf("switchback: can't find entry point\n");
      exit(1);
   }

   printf("switchback: entry bytes are: ");
   for (i = 0; i < 4; i++)
     printf("0x%02x ", (Int)entry[i]);

   LibVEX_default_VexControl(&vcon);
   vcon.guest_max_insns=1; //50;
   vcon.guest_chase_thresh=0;
   LibVEX_Init( failure_exit, log_bytes, 1, False, &vcon );
   LibVEX_Guest_initialise(&gst);

   /* set up as if a call to the entry point passing serviceFn as 
      the one and only parameter */
#  if defined(__i386__)
   gst.guest_EIP = (UInt)entry;
   gst.guest_ESP = (UInt)&gstack[25000];
   *(UInt*)(gst.guest_ESP+4) = (UInt)serviceFn;
   *(UInt*)(gst.guest_ESP+0) = 0x12345678;
#  else
#  error "Unknown arch"
#  endif

   printf("\n---START---\n");

   run_simulator();

   return 0;
}
