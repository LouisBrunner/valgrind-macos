/* isa_3_1_helpers.h */

#include "isa_3_1_register_defines.h"

extern unsigned long a_iters,b_iters,c_iters, m_iters;
extern unsigned long vrai,vrbi,vrci,vrmi;
extern unsigned long a_inc, b_inc, c_inc, m_inc;
extern unsigned long a_start, b_start, c_start, m_start;
extern unsigned long a_limit,b_limit,c_limit;
extern vector unsigned long long vrt, vra, vrb, vrc;
extern vector unsigned long long vrm;
extern vector unsigned long long vec_xs;
extern vector unsigned long long vec_xt;
extern unsigned long long dcmx;

extern unsigned long current_cr;
extern unsigned long current_fpscr;

typedef void (*test_func_t) (void);
struct test_list_t {
   test_func_t func;
   const char *name;
   const char *form;
   unsigned long mask; /* holds SP or DP indicators.  */
};
typedef struct test_list_t test_list_t;
extern struct test_list_t current_test;
typedef void (*test_group_t) (const char *name, test_func_t func,
                              unsigned int unused, char * cur_form);

/* Misc options for debug. */
/* setup_only indicates to do all of the register initializations,
   but skip the instruction test.  */
extern unsigned long setup_only;
extern int verbose;
extern unsigned long prefix_override;
extern unsigned long vrm_override;
extern unsigned long mc_override;
extern unsigned long enable_setjmp;
extern unsigned long dump_tables;
extern void debug_show_form(const char *, char *);
extern void debug_show_current_iteration();
extern void debug_dump_buffer();

extern void identify_form_components(const char *, const char *);
extern void identify_instruction_by_func_name(const char *);
extern void init_pcrelative_write_target();
extern void print_pcrelative_write_target();
extern void dump_vsxargs();
extern void generic_prologue();
extern void build_args_table();
extern void build_vsx_table();
extern void print_register_header();
extern void print_register_footer();
extern void debug_show_iter_ranges();
extern void print_result_buffer();
extern void dump_float_vsx_tables();
extern void build_float_vsx_tables();
extern void initialize_target_registers();
extern void initialize_source_registers();
extern void set_up_iterators();
extern void initialize_buffer(int);

/* This (TEXT_BSS_DELTA) is the relative distance between those
   sections as set by the linker options for the R==1 tests. */
#define TEXT_BSS_DELTA 0x20000
#define RELOC_BUFFER_SIZE 0x1000
extern unsigned long long pcrelative_buff_addr(int);
#define PAD_ORI	\
	__asm__ __volatile__ ("ori 21,21,21"         \
        :  /* empty: no outputs from asm to C   */   \
        :  /* empty: no inputs  from C   to asm */   \
        : "21"  /* clobbers register 21 */);         \
	__asm__ __volatile__ ("ori 22,22,22"         \
        :  /* empty: no outputs from asm to C   */   \
        :  /* empty: no inputs  from C   to asm */   \
        : "22"  /* clobbers register 22 */);         \
	__asm__ __volatile__ ("ori 23,23,23"         \
        :  /* empty: no outputs from asm to C   */   \
        :  /* empty: no inputs  from C   to asm */   \
        : "23"  /* clobbers register 23 */);         \
	__asm__ __volatile__ ("ori 24,24,24"         \
        :  /* empty: no outputs from asm to C   */   \
        :  /* empty: no inputs  from C   to asm */   \
        : "24"  /* clobbers register 24 */);         \
	__asm__ __volatile__ ("ori 25,25,25"         \
        :  /* empty: no outputs from asm to C   */   \
        :  /* empty: no inputs  from C   to asm */   \
        : "25"  /* clobbers register 25 */);         \
	__asm__ __volatile__ ("ori 26,26,26"         \
        :  /* empty: no outputs from asm to C   */   \
        :  /* empty: no inputs  from C   to asm */   \
        : "26"  /* clobbers register 26 */);         \
	__asm__ __volatile__ ("ori 27,27,27"         \
        :  /* empty: no outputs from asm to C   */   \
        :  /* empty: no inputs  from C   to asm */   \
        : "27"  /* clobbers register 27 */);         \
	__asm__ __volatile__ ("ori 28,28,28"         \
        :  /* empty: no outputs from asm to C   */   \
        :  /* empty: no inputs  from C   to asm */   \
        : "28"  /* clobbers register 28 */);

extern int verbose;
#define debug_printf(X) if (verbose>0) printf(X);
#define debug_show_labels (verbose>0)
#define debug_show_iters (verbose>1)
#define debug_show_values (verbose>2)
#define debug_show_all_regs (verbose>5)
#define debug_show_tables (verbose>6)
#define debug_show_raw_values (verbose>7)
#define debug_enable_all_iters (verbose>8)


#define CHECK_OVERRIDES {							\
 if (vrm_override && vrmi > 0) continue; 					\
 if (prefix_override && strncmp("p", instruction_name, 1) == 0) {		\
	if (verbose) printf("Skipping prefix insn test %s\n",instruction_name);	\
	continue;								\
	}									\
}

/* CR helpers. */

#define ALLCR "cr0","cr1","cr2","cr3","cr4","cr5","cr6","cr7"

#define SET_CR(_arg) \
      __asm__ __volatile__ ("mtcr  %0" : : "b"(_arg) : ALLCR );

#define SET_CR0_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x80,%0 " : : "b" (_arg):"cr0");
#define SET_CR1_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x40,%0 " : : "b" (_arg):"cr1");
#define SET_CR2_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x20,%0 " : : "b" (_arg):"cr2");
#define SET_CR3_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x10,%0 " : : "b" (_arg):"cr3");
#define SET_CR4_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x08,%0 " : : "r" (_arg):"cr4");
#define SET_CR5_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x04,%0 " : : "r" (_arg):"cr5");
#define SET_CR6_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x02,%0 " : : "r" (_arg):"cr6");
#define SET_CR7_FIELD(_arg) __asm__ __volatile__ ("mtocrf 0x01,%0 " : : "r" (_arg):"cr7");

#define SET_XER(_arg)   __asm__ __volatile__ ("mtxer %0" : : "b"(_arg) : "xer" );
#define GET_CR(_lval)   __asm__ __volatile__ ("mfcr %0"  : "=b"(_lval) )
#define GET_XER(_lval)  __asm__ __volatile__ ("mfxer %0" : "=b"(_lval) )
#define SET_CR_ZERO     SET_CR(0)

/* ************** */
/* FPSCR helpers. */
#define SET_FPSCR_ZERO                                        \
   do {                                                       \
      double _d = 0.0;                                        \
      __asm__ __volatile__ ("mtfsf 0xFF, %0" : : "f"(_d) );   \
   } while (0);

#define GET_FPSCR(_arg) \
  __asm__ __volatile__ ("mffs %0"  : "=f"(_arg) );


