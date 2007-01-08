/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                                      ct_fn.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Callgrind, a Valgrind tool for call tracing.

   Copyright (C) 2002-2007, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "global.h"

#define N_INITIAL_FN_ARRAY_SIZE 10071

static fn_array current_fn_active;

static Addr runtime_resolve_addr = 0;
static int  runtime_resolve_length = 0;

/* _ld_runtime_resolve, located in needs special handling:
 * The jump at end into the resolved function should not be
 * represented as a call (as usually done in callgrind with jumps),
 * but as a return + call. Otherwise, the repeated existance of
 * _ld_runtime_resolve in call chains will lead to huge cycles,
 * making the profile almost worthless.
 *
 * If ld.so is stripped, the symbol will not appear. But as this
 * function is handcrafted assembler, we search for it...
 *
 * Returns 0 if code not found, otherwise start address
 */
static void search_runtime_resolve(obj_node* obj)
{
    /* We do not check target address of <fixup>, therefore we have >1 ranges.
     * We use a tuple sequence (offset,length) into the code array for this
     */

#if defined(VGP_x86_linux)
    /* Check ranges [0-11], [16-23] */
    static int  code_offsets[] = { 0, 12, 16, 8, 24, 0 };
    static unsigned char code[] = {
	/* 0*/ 0x50, 0x51, 0x52, 0x8b, 0x54, 0x24, 0x10, 0x8b,
	/* 8*/ 0x44, 0x24, 0x0c, 0xe8, 0x70, 0x01, 0x00, 0x00,
	/*16*/ 0x5a, 0x59, 0x87, 0x04, 0x24, 0xc2, 0x08, 0x00 };
#else
#if defined(VGP_ppc32_linux)
    static int  code_offsets[] = {0, 65, 68, 64, 132, 0 };
    static unsigned char code[] = {
	/* 0*/ 0x94, 0x21, 0xff, 0xc0, 0x90, 0x01, 0x00, 0x0c,
	/* 8*/ 0x90, 0x61, 0x00, 0x10, 0x90, 0x81, 0x00, 0x14,
	/*16*/ 0x7d, 0x83, 0x63, 0x78, 0x90, 0xa1, 0x00, 0x18,
	/*24*/ 0x7d, 0x64, 0x5b, 0x78, 0x90, 0xc1, 0x00, 0x1c,
	/*32*/ 0x7c, 0x08, 0x02, 0xa6, 0x90, 0xe1, 0x00, 0x20,
	/*40*/ 0x90, 0x01, 0x00, 0x30, 0x91, 0x01, 0x00, 0x24,
	/*48*/ 0x7c, 0x00, 0x00, 0x26, 0x91, 0x21, 0x00, 0x28,
	/*56*/ 0x91, 0x41, 0x00, 0x2c, 0x90, 0x01, 0x00, 0x08,
	/*64*/ 0x48, 0x00, 0x02, 0x91, 0x7c, 0x69, 0x03, 0xa6, /* at 64: bl aff0 <fixup> */
	/*72*/ 0x80, 0x01, 0x00, 0x30, 0x81, 0x41, 0x00, 0x2c,
	/*80*/ 0x81, 0x21, 0x00, 0x28, 0x7c, 0x08, 0x03, 0xa6,
	/*88*/ 0x81, 0x01, 0x00, 0x24, 0x80, 0x01, 0x00, 0x08,
	/*96*/ 0x80, 0xe1, 0x00, 0x20, 0x80, 0xc1, 0x00, 0x1c,
	/*104*/0x7c, 0x0f, 0xf1, 0x20, 0x80, 0xa1, 0x00, 0x18,
	/*112*/0x80, 0x81, 0x00, 0x14, 0x80, 0x61, 0x00, 0x10,
	/*120*/0x80, 0x01, 0x00, 0x0c, 0x38, 0x21, 0x00, 0x40,
	/*128*/0x4e, 0x80, 0x04, 0x20 };
#else
#if defined(VGP_amd64_linux)
    /* x86_64 */
    static int  code_offsets[] = {0, 62, 66, 44, 110, 0 };
    static unsigned char code[] = {
	/* 0*/ 0x48, 0x83, 0xec, 0x38, 0x48, 0x89, 0x04, 0x24,
	/* 8*/ 0x48, 0x89, 0x4c, 0x24, 0x08, 0x48, 0x89, 0x54, 0x24, 0x10,
	/*18*/ 0x48, 0x89, 0x74, 0x24, 0x18, 0x48, 0x89, 0x7c, 0x24, 0x20,
	/*28*/ 0x4c, 0x89, 0x44, 0x24, 0x28, 0x4c, 0x89, 0x4c, 0x24, 0x30,
	/*38*/ 0x48, 0x8b, 0x74, 0x24, 0x40, 0x49, 0x89, 0xf3,
	/*46*/ 0x4c, 0x01, 0xde, 0x4c, 0x01, 0xde, 0x48, 0xc1, 0xe6, 0x03,
	/*56*/ 0x48, 0x8b, 0x7c, 0x24, 0x38, 0xe8, 0xee, 0x01, 0x00, 0x00,
	/*66*/ 0x49, 0x89, 0xc3, 0x4c, 0x8b, 0x4c, 0x24, 0x30,
	/*74*/ 0x4c, 0x8b, 0x44, 0x24, 0x28, 0x48, 0x8b, 0x7c, 0x24, 0x20,
	/*84*/ 0x48, 0x8b, 0x74, 0x24, 0x18, 0x48, 0x8b, 0x54, 0x24, 0x10,
	/*94*/ 0x48, 0x8b, 0x4c, 0x24, 0x08, 0x48, 0x8b, 0x04, 0x24,
	/*103*/0x48, 0x83, 0xc4, 0x48, 0x41, 0xff, 0xe3 };
#else
    /* Unknown platform, no check is done */
    static int  code_offsets[] = {0, 0 };
    static unsigned char code[] = { 0 };
#endif
#endif
#endif
    
    int *range = &(code_offsets[0]), *r = 0;
    Bool found = False;
    Addr addr, end;

    /* Only search in libraries with a given name pattern */
    if ((VG_(strncmp)(obj->name, "/lib/ld", 7) != 0) &&
	(VG_(strncmp)(obj->name, "/lib64/ld", 9) != 0)) return;
    
    CLG_DEBUG(1, "search_rs: Checking %d bytes of [%x %x %x...]\n",
	      range[1], code[0], code[1], code[2]);

    end = obj->start + obj->size - range[1];
    addr = obj->start;

    if (range[1] == 0) return;

    while(addr < end) {
	if (VG_(memcmp)( (void*)addr, code, range[1]) == 0) {

	    r = range + 2;
	    found = True;
	    while(r[1]) {
		CLG_DEBUG(1, " [%p] Found! Checking %d bytes of [%x %x %x...]\n",
			  addr, r[1], code[r[0]], code[r[0]+1], code[r[0]+2]);

		if (VG_(memcmp)( (void*)(addr+r[0]), code+r[0], r[1]) != 0) {
		    found = False;
		    break;
		}
		r += 2;
	    }
	    if (found) break;
	}
	addr++;
    }

    if (!found || (r==0)) return;

    if (VG_(clo_verbosity) > 1)
	VG_(message)(Vg_DebugMsg, "Code check found runtime_resolve: %s +%p=%p, length %d",
		     obj->name + obj->last_slash_pos,
		     addr - obj->start, addr, r[0]);

    runtime_resolve_addr   = addr;
    runtime_resolve_length = r[0];
}

/*------------------------------------------------------------*/
/*--- Object/File/Function hash entry operations           ---*/
/*------------------------------------------------------------*/

/* Object hash table, fixed */
static obj_node* obj_table[N_OBJ_ENTRIES];

void CLG_(init_obj_table)()
{
    Int i;
    for (i = 0; i < N_OBJ_ENTRIES; i++)
	obj_table[i] = 0;
}

#define HASH_CONSTANT   256

static UInt str_hash(const Char *s, UInt table_size)
{
    int hash_value = 0;
    for ( ; *s; s++)
        hash_value = (HASH_CONSTANT * hash_value + *s) % table_size;
    return hash_value;
}


static Char* anonymous_obj = "???";

static __inline__ 
obj_node* new_obj_node(SegInfo* si, obj_node* next)
{
   Int i;
   obj_node* new;

   new = (obj_node*) CLG_MALLOC(sizeof(obj_node));
   new->name  = si ? VG_(strdup)( VG_(seginfo_filename)(si) )
                     : anonymous_obj;
   for (i = 0; i < N_FILE_ENTRIES; i++) {
      new->files[i] = NULL;
   }
   CLG_(stat).distinct_objs ++;
   new->number  = CLG_(stat).distinct_objs;
   new->start   = si ? VG_(seginfo_start)(si) : 0;
   new->size    = si ? VG_(seginfo_size)(si) : 0;
   new->offset  = si ? VG_(seginfo_sym_offset)(si) : 0;
   new->next    = next;

   // not only used for debug output (see static.c)
   new->last_slash_pos = 0;
   i = 0;
   while(new->name[i]) {
	if (new->name[i]=='/') new->last_slash_pos = i+1;
	i++;
   }

   if (runtime_resolve_addr == 0) search_runtime_resolve(new);
   
   return new;
}

obj_node* CLG_(get_obj_node)(SegInfo* si)
{
    obj_node*    curr_obj_node;
    UInt         objname_hash;
    const UChar* obj_name;
    
    obj_name = si ? (Char*) VG_(seginfo_filename)(si) : anonymous_obj;

    /* lookup in obj hash */
    objname_hash = str_hash(obj_name, N_OBJ_ENTRIES);
    curr_obj_node = obj_table[objname_hash];
    while (NULL != curr_obj_node && 
	   VG_(strcmp)(obj_name, curr_obj_node->name) != 0) {
	curr_obj_node = curr_obj_node->next;
    }
    if (NULL == curr_obj_node) {
	obj_table[objname_hash] = curr_obj_node = 
	    new_obj_node(si, obj_table[objname_hash]);
    }

    return curr_obj_node;
}


static __inline__ 
file_node* new_file_node(Char filename[FILENAME_LEN],
			 obj_node* obj, file_node* next)
{
  Int i;
  file_node* new = (file_node*) CLG_MALLOC(sizeof(file_node));
  new->name  = VG_(strdup)(filename);
  for (i = 0; i < N_FN_ENTRIES; i++) {
    new->fns[i] = NULL;
  }
  CLG_(stat).distinct_files++;
  new->number  = CLG_(stat).distinct_files;
  new->obj     = obj;
  new->next      = next;
  return new;
}

 
file_node* CLG_(get_file_node)(obj_node* curr_obj_node,
			      Char filename[FILENAME_LEN])
{
    file_node* curr_file_node;
    UInt       filename_hash;

    /* lookup in file hash */
    filename_hash = str_hash(filename, N_FILE_ENTRIES);
    curr_file_node = curr_obj_node->files[filename_hash];
    while (NULL != curr_file_node && 
	   VG_(strcmp)(filename, curr_file_node->name) != 0) {
	curr_file_node = curr_file_node->next;
    }
    if (NULL == curr_file_node) {
	curr_obj_node->files[filename_hash] = curr_file_node = 
	    new_file_node(filename, curr_obj_node, 
			  curr_obj_node->files[filename_hash]);
    }

    return curr_file_node;
}

/* forward decl. */
static void resize_fn_array(void);

static __inline__ 
fn_node* new_fn_node(Char fnname[FILENAME_LEN],
		     file_node* file, fn_node* next)
{
    fn_node* new = (fn_node*) CLG_MALLOC(sizeof(fn_node));
    new->name = VG_(strdup)(fnname);

    CLG_(stat).distinct_fns++;
    new->number   = CLG_(stat).distinct_fns;
    new->last_cxt = 0;
    new->pure_cxt = 0;
    new->file     = file;
    new->next     = next;

    new->dump_before  = False;
    new->dump_after   = False;
    new->zero_before  = False;
    new->toggle_collect = False;
    new->skip         = False;
    new->pop_on_jump  = CLG_(clo).pop_on_jump;
    new->is_malloc    = False;
    new->is_realloc   = False;
    new->is_free      = False;

    new->group        = 0;
    new->separate_callers    = CLG_(clo).separate_callers;
    new->separate_recursions = CLG_(clo).separate_recursions;

#if CLG_ENABLE_DEBUG
    new->verbosity    = -1;
#endif

    if (CLG_(stat).distinct_fns >= current_fn_active.size)
	resize_fn_array();

    return new;
}


/* Get a function node in hash2 with known file node.
 * hash nodes are created if needed
 */
static
fn_node* get_fn_node_infile(file_node* curr_file_node,
			    Char fnname[FN_NAME_LEN])
{
    fn_node* curr_fn_node;
    UInt     fnname_hash;

    CLG_ASSERT(curr_file_node != 0);

    /* lookup in function hash */
    fnname_hash = str_hash(fnname, N_FN_ENTRIES);
    curr_fn_node = curr_file_node->fns[fnname_hash];
    while (NULL != curr_fn_node && 
	   VG_(strcmp)(fnname, curr_fn_node->name) != 0) {
	curr_fn_node = curr_fn_node->next;
    }
    if (NULL == curr_fn_node) {
	curr_file_node->fns[fnname_hash] = curr_fn_node = 
            new_fn_node(fnname, curr_file_node,
			curr_file_node->fns[fnname_hash]);
    }

    return curr_fn_node;
}


/* Get a function node in a Segment.
 * Hash nodes are created if needed.
 */
static __inline__
fn_node* get_fn_node_inseg(SegInfo* si,
			   Char filename[FILENAME_LEN],
			   Char fnname[FN_NAME_LEN])
{
  obj_node  *obj  = CLG_(get_obj_node)(si);
  file_node *file = CLG_(get_file_node)(obj, filename);
  fn_node   *fn   = get_fn_node_infile(file, fnname);

  return fn;
}


Bool CLG_(get_debug_info)(Addr instr_addr,
			 Char filename[FILENAME_LEN],
			 Char fn_name[FN_NAME_LEN], UInt* line_num,
			 SegInfo** pSegInfo)
{
  Bool found1, found2, result = True;
  UInt line;
  
  CLG_DEBUG(6, "  + get_debug_info(%p)\n", instr_addr);

  if (pSegInfo) {
      *pSegInfo = VG_(find_seginfo)(instr_addr);

      // for generated code in anonymous space, pSegInfo is 0
   }

   found1 = VG_(get_filename_linenum)(instr_addr,
				      filename, FILENAME_LEN,
				      NULL, 0, NULL, // FIXME: add dirnames!
				      &line);
   found2 = VG_(get_fnname)(instr_addr, 
			    fn_name, FN_NAME_LEN);

   if (!found1 && !found2) {
     CLG_(stat).no_debug_BBs++;
     VG_(strcpy)(filename, "???");
     VG_(strcpy)(fn_name,  "???");
     if (line_num) *line_num=0;
     result = False;

   } else if ( found1 &&  found2) {
     CLG_(stat).full_debug_BBs++;
     if (line_num) *line_num=line;

   } else if ( found1 && !found2) {
     CLG_(stat).file_line_debug_BBs++;
     VG_(strcpy)(fn_name,  "???");
     if (line_num) *line_num=line;

   } else  /*(!found1 &&  found2)*/ {
     CLG_(stat).fn_name_debug_BBs++;
     VG_(strcpy)(filename, "???");
     if (line_num) *line_num=0;
   }

   CLG_DEBUG(6, "  - get_debug_info(%p): seg '%s', fn %s\n",
	    instr_addr,
	    !pSegInfo   ? (const UChar*)"-" :
	    (*pSegInfo) ? VG_(seginfo_filename)(*pSegInfo) :
	    (const UChar*)"(None)",
	    fn_name);

  return result;
}

/* for _libc_freeres_wrapper => _exit renaming */
static BB* exit_bb = 0;


/*
 * Attach function struct to a BB from debug info.
 */
fn_node* CLG_(get_fn_node)(BB* bb)
{
    Char       filename[FILENAME_LEN], fnname[FN_NAME_LEN];
    SegInfo*   si;
    UInt       line_num;
    fn_node*   fn;

    /* fn from debug info is idempotent for a BB */
    if (bb->fn) return bb->fn;

    CLG_DEBUG(3,"+ get_fn_node(BB %p)\n", bb_addr(bb));

    /* get function/file name, line number and object of
     * the BB according to debug information
     */
    CLG_(get_debug_info)(bb_addr(bb),
			filename, fnname, &line_num, &si);

    if (0 == VG_(strcmp)(fnname, "???")) {
	int p;

	/* Use address as found in library */
	if (sizeof(Addr) == 4)
	    p = VG_(sprintf)(fnname, "%08p", bb->offset);
	else 	    
	    // 64bit address
	    p = VG_(sprintf)(fnname, "%016p", bb->offset);

	VG_(sprintf)(fnname+p, "%s", 
		     (bb->sect_kind == Vg_SectData) ? " [Data]" :
		     (bb->sect_kind == Vg_SectBSS)  ? " [BSS]"  :
		     (bb->sect_kind == Vg_SectGOT)  ? " [GOT]"  :
		     (bb->sect_kind == Vg_SectPLT)  ? " [PLT]"  : "");
    }
    else {
      if (VG_(get_fnname_if_entry)(bb_addr(bb), fnname, FN_NAME_LEN))
	bb->is_entry = 1;
    }

    /* HACK for correct _exit: 
     * _exit is redirected to VG_(__libc_freeres_wrapper) by valgrind,
     * so we rename it back again :-)
     */
    if (0 == VG_(strcmp)(fnname, "vgPlain___libc_freeres_wrapper")
	&& exit_bb) {
      CLG_(get_debug_info)(bb_addr(exit_bb),
			  filename, fnname, &line_num, &si);
	
	CLG_DEBUG(1, "__libc_freeres_wrapper renamed to _exit\n");
    }
    if (0 == VG_(strcmp)(fnname, "_exit") && !exit_bb)
	exit_bb = bb;
    
    if (runtime_resolve_addr && 
	(bb_addr(bb) >= runtime_resolve_addr) &&
	(bb_addr(bb) < runtime_resolve_addr + runtime_resolve_length)) {
	/* BB in runtime_resolve found by code check; use this name */
	VG_(sprintf)(fnname, "_dl_runtime_resolve");
    }

    /* get fn_node struct for this function */
    fn = get_fn_node_inseg( si, filename, fnname);

    /* if this is the 1st time the function is seen,
     * some attributes are set */
    if (fn->pure_cxt == 0) {

      /* Every function gets a "pure" context, i.e. a context with stack
       * depth 1 only with this function. This is for compression of mangled
       * names
       */
      fn_node* pure[2];
      pure[0] = 0;
      pure[1] = fn;
      fn->pure_cxt = CLG_(get_cxt)(pure+1);

      if (bb->sect_kind == Vg_SectPLT)	
	fn->skip = CLG_(clo).skip_plt;

      if (VG_(strcmp)(fn->name, "_dl_runtime_resolve")==0) {
	  fn->pop_on_jump = True;

	  if (VG_(clo_verbosity) > 1)
	      VG_(message)(Vg_DebugMsg, "Symbol match: found runtime_resolve: %s +%p=%p",
		      bb->obj->name + bb->obj->last_slash_pos,
		      bb->offset, bb_addr(bb));
      }

      fn->is_malloc  = (VG_(strcmp)(fn->name, "malloc")==0);
      fn->is_realloc = (VG_(strcmp)(fn->name, "realloc")==0);
      fn->is_free    = (VG_(strcmp)(fn->name, "free")==0);

      /* apply config options from function name patterns
       * given on command line */
      CLG_(update_fn_config)(fn);
    }


    bb->fn   = fn;
    bb->line = line_num;

    CLG_DEBUG(3,"- get_fn_node(BB %p): %s (in %s:%u)\n",
	     bb_addr(bb), fnname, filename, line_num);

    return fn;
}


/*------------------------------------------------------------*/
/*--- Active function array operations                     ---*/
/*------------------------------------------------------------*/

/* The active function array is a thread-specific array
 * of UInts, mapping function numbers to the active count of
 * functions.
 * The active count is the number of times a function appears
 * in the current call stack, and is used when costs for recursion
 * levels should be separated.
 */

UInt* CLG_(get_fn_entry)(Int n)
{
  CLG_ASSERT(n < current_fn_active.size);
  return current_fn_active.array + n;
}

void CLG_(init_fn_array)(fn_array* a)
{
  Int i;

  CLG_ASSERT(a != 0);

  a->size = N_INITIAL_FN_ARRAY_SIZE;
  if (a->size <= CLG_(stat).distinct_fns)
    a->size = CLG_(stat).distinct_fns+1;
  
  a->array = (UInt*) CLG_MALLOC(a->size * sizeof(UInt));
  for(i=0;i<a->size;i++)
    a->array[i] = 0;
}

void CLG_(copy_current_fn_array)(fn_array* dst)
{
  CLG_ASSERT(dst != 0);

  dst->size  = current_fn_active.size;
  dst->array = current_fn_active.array;
}

fn_array* CLG_(get_current_fn_array)()
{
  return &current_fn_active;
}

void CLG_(set_current_fn_array)(fn_array* a)
{
  CLG_ASSERT(a != 0);

  current_fn_active.size  = a->size;
  current_fn_active.array = a->array;
  if (current_fn_active.size <= CLG_(stat).distinct_fns)
    resize_fn_array();
}

/* ensure that active_array is big enough:
 *  <distinct_fns> is the highest index, so <fn_active_array_size>
 *  has to be bigger than that.
 */
static void resize_fn_array(void)
{
    UInt* new;
    Int i, newsize;

    newsize = current_fn_active.size;
    while (newsize <= CLG_(stat).distinct_fns) newsize *=2;

    CLG_DEBUG(0, "Resize fn_active_array: %d => %d\n",
	     current_fn_active.size, newsize);

    new = (UInt*) CLG_MALLOC(newsize * sizeof(UInt));
    for(i=0;i<current_fn_active.size;i++)
      new[i] = current_fn_active.array[i];
    while(i<newsize)
	new[i++] = 0;

    VG_(free)(current_fn_active.array);
    current_fn_active.size = newsize;
    current_fn_active.array = new;
    CLG_(stat).fn_array_resizes++;
}


