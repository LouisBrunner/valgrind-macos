/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                                      ct_fn.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Callgrind, a Valgrind tool for call tracing.

   Copyright (C) 2002-2013, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

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

// a code pattern is a list of tuples (start offset, length)
struct chunk_t { int start, len; };
struct pattern
{
    const HChar* name;
    int len;
    struct chunk_t chunk[];
};

/* Scan for a pattern in the code of an ELF object.
 * If found, return true and set runtime_resolve_{addr,length}
 */
__attribute__((unused))    // Possibly;  depends on the platform.
static Bool check_code(obj_node* obj,
                       UChar code[], struct pattern* pat)
{
    Bool found;
    Addr addr, end;
    int chunk, start, len;

    /* first chunk of pattern should always start at offset 0 and
     * have at least 3 bytes */
    CLG_ASSERT((pat->chunk[0].start == 0) && (pat->chunk[0].len >2));
    
    CLG_DEBUG(1, "check_code: %s, pattern %s, check %d bytes of [%x %x %x...]\n",
              obj->name, pat->name, pat->chunk[0].len, code[0], code[1], code[2]);

    end = obj->start + obj->size - pat->len;
    addr = obj->start;
    while(addr < end) {
	found = (VG_(memcmp)( (void*)addr, code, pat->chunk[0].len) == 0);

        if (found) {
	    chunk = 1;
	    while(1) {		
		start = pat->chunk[chunk].start;
		len   = pat->chunk[chunk].len;
		if (len == 0) break;

		CLG_ASSERT(len >2);
                CLG_DEBUG(1, " found chunk %d at %#lx, checking %d bytes "
                             "of [%x %x %x...]\n",
                          chunk-1, addr - obj->start, len,
			  code[start], code[start+1], code[start+2]);

                if (VG_(memcmp)( (void*)(addr+start), code+start, len) != 0) {
                    found = False;
                    break;
                }
		chunk++;
	    }

            if (found) {
		CLG_DEBUG(1, "found at offset %#lx.\n", addr - obj->start);
		if (VG_(clo_verbosity) > 1)
		    VG_(message)(Vg_DebugMsg, "Found runtime_resolve (%s): "
                                              "%s +%#lx=%#lx, length %d\n",
				 pat->name, obj->name + obj->last_slash_pos,
				 addr - obj->start, addr, pat->len);
		    
		runtime_resolve_addr   = addr;
		runtime_resolve_length = pat->len;
		return True;
	    }
        }
        addr++;
    }
    CLG_DEBUG(1, " found nothing.\n");
    return False;
}


/* _ld_runtime_resolve, located in ld.so, needs special handling:
 * The jump at end into the resolved function should not be
 * represented as a call (as usually done in callgrind with jumps),
 * but as a return + call. Otherwise, the repeated existance of
 * _ld_runtime_resolve in call chains will lead to huge cycles,
 * making the profile almost worthless.
 *
 * If ld.so is stripped, the symbol will not appear. But as this
 * function is handcrafted assembler, we search for it.
 *
 * We stop if the ELF object name does not seem to be the runtime linker
 */
static Bool search_runtime_resolve(obj_node* obj)
{
#if defined(VGP_x86_linux)
    static UChar code[] = {
	/* 0*/ 0x50, 0x51, 0x52, 0x8b, 0x54, 0x24, 0x10, 0x8b,
	/* 8*/ 0x44, 0x24, 0x0c, 0xe8, 0x70, 0x01, 0x00, 0x00,
	/*16*/ 0x5a, 0x59, 0x87, 0x04, 0x24, 0xc2, 0x08, 0x00 };
    /* Check ranges [0-11] and [16-23] ([12-15] is an absolute address) */
    static struct pattern pat = {
	"x86-def", 24, {{ 0,12 }, { 16,8 }, { 24,0}} };

    /* Pattern for glibc-2.8 on OpenSuse11.0 */
    static UChar code_28[] = {
	/* 0*/ 0x50, 0x51, 0x52, 0x8b, 0x54, 0x24, 0x10, 0x8b,
	/* 8*/ 0x44, 0x24, 0x0c, 0xe8, 0x70, 0x01, 0x00, 0x00,
	/*16*/ 0x5a, 0x8b, 0x0c, 0x24, 0x89, 0x04, 0x24, 0x8b,
	/*24*/ 0x44, 0x24, 0x04, 0xc2, 0x0c, 0x00 };
    static struct pattern pat_28 = {
	"x86-glibc2.8", 30, {{ 0,12 }, { 16,14 }, { 30,0}} };

    if (VG_(strncmp)(obj->name, "/lib/ld", 7) != 0) return False;
    if (check_code(obj, code, &pat)) return True;
    if (check_code(obj, code_28, &pat_28)) return True;
    return False;
#endif

#if defined(VGP_ppc32_linux)
    static UChar code[] = {
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
    static struct pattern pat = {
	"ppc32-def", 132, {{ 0,65 }, { 68,64 }, { 132,0 }} };

    if (VG_(strncmp)(obj->name, "/lib/ld", 7) != 0) return False;
    return check_code(obj, code, &pat);
#endif

#if defined(VGP_amd64_linux)
    static UChar code[] = {
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
    static struct pattern pat = {
	"amd64-def", 110, {{ 0,62 }, { 66,44 }, { 110,0 }} };

    if ((VG_(strncmp)(obj->name, "/lib/ld", 7) != 0) &&
	(VG_(strncmp)(obj->name, "/lib64/ld", 9) != 0)) return False;
    return check_code(obj, code, &pat);
#endif

    /* For other platforms, no patterns known */
    return False;
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

static UInt str_hash(const HChar *s, UInt table_size)
{
    int hash_value = 0;
    for ( ; *s; s++)
        hash_value = (HASH_CONSTANT * hash_value + *s) % table_size;
    return hash_value;
}


static const HChar* anonymous_obj = "???";

static __inline__ 
obj_node* new_obj_node(DebugInfo* di, obj_node* next)
{
   Int i;
   obj_node* obj;

   obj = (obj_node*) CLG_MALLOC("cl.fn.non.1", sizeof(obj_node));
   obj->name  = di ? VG_(strdup)( "cl.fn.non.2",
                                  VG_(DebugInfo_get_filename)(di) )
                   : anonymous_obj;
   for (i = 0; i < N_FILE_ENTRIES; i++) {
      obj->files[i] = NULL;
   }
   CLG_(stat).distinct_objs ++;
   obj->number  = CLG_(stat).distinct_objs;
   /* JRS 2008 Feb 19: maybe rename .start/.size/.offset to
      .text_avma/.text_size/.test_bias to make it clearer what these
      fields really mean */
   obj->start   = di ? VG_(DebugInfo_get_text_avma)(di) : 0;
   obj->size    = di ? VG_(DebugInfo_get_text_size)(di) : 0;
   obj->offset  = di ? VG_(DebugInfo_get_text_bias)(di) : 0;
   obj->next    = next;

   // not only used for debug output (see static.c)
   obj->last_slash_pos = 0;
   i = 0;
   while(obj->name[i]) {
	if (obj->name[i]=='/') obj->last_slash_pos = i+1;
	i++;
   }

   if (runtime_resolve_addr == 0) search_runtime_resolve(obj);

   return obj;
}

obj_node* CLG_(get_obj_node)(DebugInfo* di)
{
    obj_node*    curr_obj_node;
    UInt         objname_hash;
    const HChar* obj_name;
    
    obj_name = di ? VG_(DebugInfo_get_filename)(di) : anonymous_obj;

    /* lookup in obj hash */
    objname_hash = str_hash(obj_name, N_OBJ_ENTRIES);
    curr_obj_node = obj_table[objname_hash];
    while (NULL != curr_obj_node && 
	   VG_(strcmp)(obj_name, curr_obj_node->name) != 0) {
	curr_obj_node = curr_obj_node->next;
    }
    if (NULL == curr_obj_node) {
	obj_table[objname_hash] = curr_obj_node = 
	    new_obj_node(di, obj_table[objname_hash]);
    }

    return curr_obj_node;
}


static __inline__ 
file_node* new_file_node(HChar filename[FILENAME_LEN],
			 obj_node* obj, file_node* next)
{
  Int i;
  file_node* file = (file_node*) CLG_MALLOC("cl.fn.nfn.1",
                                           sizeof(file_node));
  file->name  = VG_(strdup)("cl.fn.nfn.2", filename);
  for (i = 0; i < N_FN_ENTRIES; i++) {
    file->fns[i] = NULL;
  }
  CLG_(stat).distinct_files++;
  file->number  = CLG_(stat).distinct_files;
  file->obj     = obj;
  file->next      = next;
  return file;
}

 
file_node* CLG_(get_file_node)(obj_node* curr_obj_node,
                               HChar filename[FILENAME_LEN])
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
fn_node* new_fn_node(HChar fnname[FILENAME_LEN],
		     file_node* file, fn_node* next)
{
    fn_node* fn = (fn_node*) CLG_MALLOC("cl.fn.nfnnd.1",
                                         sizeof(fn_node));
    fn->name = VG_(strdup)("cl.fn.nfnnd.2", fnname);

    CLG_(stat).distinct_fns++;
    fn->number   = CLG_(stat).distinct_fns;
    fn->last_cxt = 0;
    fn->pure_cxt = 0;
    fn->file     = file;
    fn->next     = next;

    fn->dump_before  = False;
    fn->dump_after   = False;
    fn->zero_before  = False;
    fn->toggle_collect = False;
    fn->skip         = False;
    fn->pop_on_jump  = CLG_(clo).pop_on_jump;
    fn->is_malloc    = False;
    fn->is_realloc   = False;
    fn->is_free      = False;

    fn->group        = 0;
    fn->separate_callers    = CLG_(clo).separate_callers;
    fn->separate_recursions = CLG_(clo).separate_recursions;

#if CLG_ENABLE_DEBUG
    fn->verbosity    = -1;
#endif

    if (CLG_(stat).distinct_fns >= current_fn_active.size)
	resize_fn_array();

    return fn;
}


/* Get a function node in hash2 with known file node.
 * hash nodes are created if needed
 */
static
fn_node* get_fn_node_infile(file_node* curr_file_node,
			    HChar fnname[FN_NAME_LEN])
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
fn_node* get_fn_node_inseg(DebugInfo* di,
			   HChar filename[FILENAME_LEN],
			   HChar fnname[FN_NAME_LEN])
{
  obj_node  *obj  = CLG_(get_obj_node)(di);
  file_node *file = CLG_(get_file_node)(obj, filename);
  fn_node   *fn   = get_fn_node_infile(file, fnname);

  return fn;
}


Bool CLG_(get_debug_info)(Addr instr_addr,
			 HChar file[FILENAME_LEN],
			 HChar fn_name[FN_NAME_LEN], UInt* line_num,
			 DebugInfo** pDebugInfo)
{
  Bool found_file_line, found_fn, found_dirname, result = True;
  HChar dir[FILENAME_LEN];
  UInt line;
  
  CLG_DEBUG(6, "  + get_debug_info(%#lx)\n", instr_addr);

  if (pDebugInfo) {
      *pDebugInfo = VG_(find_DebugInfo)(instr_addr);

      // for generated code in anonymous space, pSegInfo is 0
   }

   found_file_line = VG_(get_filename_linenum)(instr_addr,
					       file, FILENAME_LEN,
					       dir, FILENAME_LEN,
					       &found_dirname,
					       &line);
   found_fn = VG_(get_fnname)(instr_addr,
			      fn_name, FN_NAME_LEN);

   if (found_dirname) {
       // +1 for the '/'.
       CLG_ASSERT(VG_(strlen)(dir) + VG_(strlen)(file) + 1 < FILENAME_LEN);
       VG_(strcat)(dir, "/");         // Append '/'
       VG_(strcat)(dir, file);    // Append file to dir
       VG_(strcpy)(file, dir);    // Move dir+file to file
   }

   if (!found_file_line && !found_fn) {
     CLG_(stat).no_debug_BBs++;
     VG_(strcpy)(file, "???");
     VG_(strcpy)(fn_name,  "???");
     if (line_num) *line_num=0;
     result = False;

   } else if ( found_file_line &&  found_fn) {
     CLG_(stat).full_debug_BBs++;
     if (line_num) *line_num=line;

   } else if ( found_file_line && !found_fn) {
     CLG_(stat).file_line_debug_BBs++;
     VG_(strcpy)(fn_name,  "???");
     if (line_num) *line_num=line;

   } else  /*(!found_file_line &&  found_fn)*/ {
     CLG_(stat).fn_name_debug_BBs++;
     VG_(strcpy)(file, "???");
     if (line_num) *line_num=0;
   }

   CLG_DEBUG(6, "  - get_debug_info(%#lx): seg '%s', fn %s\n",
	    instr_addr,
	    !pDebugInfo   ? "-" :
	    (*pDebugInfo) ? VG_(DebugInfo_get_filename)(*pDebugInfo) :
	    "(None)",
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
    HChar      filename[FILENAME_LEN], fnname[FN_NAME_LEN];
    DebugInfo* di;
    UInt       line_num;
    fn_node*   fn;

    /* fn from debug info is idempotent for a BB */
    if (bb->fn) return bb->fn;

    CLG_DEBUG(3,"+ get_fn_node(BB %#lx)\n", bb_addr(bb));

    /* get function/file name, line number and object of
     * the BB according to debug information
     */
    CLG_(get_debug_info)(bb_addr(bb),
			filename, fnname, &line_num, &di);

    if (0 == VG_(strcmp)(fnname, "???")) {
	int p;

	/* Use address as found in library */
	if (sizeof(Addr) == 4)
	    p = VG_(sprintf)(fnname, "%#08lx", bb->offset);
	else 	    
	    // 64bit address
	    p = VG_(sprintf)(fnname, "%#016lx", bb->offset);

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
			  filename, fnname, &line_num, &di);
	
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
    fn = get_fn_node_inseg( di, filename, fnname);

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
	      VG_(message)(Vg_DebugMsg, "Symbol match: found runtime_resolve:"
                                        " %s +%#lx=%#lx\n",
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

    CLG_DEBUG(3,"- get_fn_node(BB %#lx): %s (in %s:%u)\n",
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
  
  a->array = (UInt*) CLG_MALLOC("cl.fn.gfe.1",
                                a->size * sizeof(UInt));
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
    UInt* new_array;
    Int i, newsize;

    newsize = current_fn_active.size;
    while (newsize <= CLG_(stat).distinct_fns) newsize *=2;

    CLG_DEBUG(0, "Resize fn_active_array: %d => %d\n",
	     current_fn_active.size, newsize);

    new_array = (UInt*) CLG_MALLOC("cl.fn.rfa.1", newsize * sizeof(UInt));
    for(i=0;i<current_fn_active.size;i++)
      new_array[i] = current_fn_active.array[i];
    while(i<newsize)
	new_array[i++] = 0;

    VG_(free)(current_fn_active.array);
    current_fn_active.size = newsize;
    current_fn_active.array = new_array;
    CLG_(stat).fn_array_resizes++;
}


