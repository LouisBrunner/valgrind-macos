#ifndef _COREGRIND_UME_H
#define _COREGRIND_UME_H

#include <elf.h>
#include <sys/types.h>

#if	ELFSZ == 64
#define ESZ(x)	Elf64_##x
#elif	ELFSZ == 32
#define ESZ(x)	Elf32_##x
#else
#error ELFSZ needs to ==32 or ==64
#endif

/* Integer type the same size as a pointer */
typedef ESZ(Addr) addr_t;

struct exeinfo
{
   int		setbrk;		/* INPUT: if true, set the brk segment base */
   addr_t	map_base;	/* INPUT: if non-zero, base address of mappings  */

   addr_t	exe_base;	/* INOUT: lowest (allowed) address of exe	*/
   addr_t	exe_end;	/* INOUT: highest (allowed) address	*/

   addr_t	phdr;		/* address phdr was mapped at		*/
   int		phnum;		/* number of phdrs			*/
   addr_t	interp_base;	/* where interpreter (ld.so) was mapped	*/
   addr_t	entry;		/* entrypoint in main executable	*/
   addr_t	init_eip;	/* initial eip				*/
   addr_t	brkbase;	/* base address of brk segment		*/

   /* these are the extra args added by #! scripts */
   char		*argv0;		/* the interpreter name */
   char		*argv1;		/* the args for the interpreter */

   char		**argv;		/* the original argv */
};

int do_exec(const char *exe, struct exeinfo *info);

void foreach_map(int (*fn)(void *start, void *end,
			   const char *perm, off_t offset,
			   int maj, int min, int ino));
void as_pad(void *start, void *end);
void as_unpad(void *start, void *end);
void as_closepadfile(void);
int  as_getpadfd(void);
void as_setpadfd(int);

struct elfinfo
{
   ESZ(Ehdr)	e;
   ESZ(Phdr)	*p;
   int		fd;
};

struct elfinfo *readelf(int fd, const char *filename);
ESZ(Addr) mapelf(struct elfinfo *e, ESZ(Addr) base, int setbrk);

struct ume_auxv
{
   int	a_type;
   union {
      void *a_ptr;
      int   a_val;
      void (*a_fcn)(void);
   };
};

struct ume_auxv *find_auxv(int *orig_esp);

/* Our private auxv entries */
#define AT_UME_PADFD	0xff01	/* padding file fd */
#define AT_UME_EXECFD	0xff02	/* stage1 executable fd */

#endif /* _COREGRIND_UME_H */
