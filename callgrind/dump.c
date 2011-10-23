/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                                       dump.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Callgrind, a Valgrind tool for call tracing.

   Copyright (C) 2002-2011, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

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

#include "config.h"
#include "global.h"

#include "pub_tool_threadstate.h"
#include "pub_tool_libcfile.h"


/* Dump Part Counter */
static Int out_counter = 0;

static Char* out_file = 0;
static Char* out_directory = 0;
static Bool dumps_initialized = False;

/* Command */
static Char cmdbuf[BUF_LEN];

/* Total reads/writes/misses sum over all dumps and threads.
 * Updated during CC traversal at dump time.
 */
FullCost CLG_(total_cost) = 0;
static FullCost dump_total_cost = 0;

EventMapping* CLG_(dumpmap) = 0;

/* Temporary output buffer for
 *  print_fn_pos, fprint_apos, fprint_fcost, fprint_jcc,
 *  fprint_fcc_ln, dump_run_info, dump_state_info
 */
static Char outbuf[FILENAME_LEN + FN_NAME_LEN + OBJ_NAME_LEN + COSTS_LEN];

Int CLG_(get_dump_counter)(void)
{
  return out_counter;
}

Char* CLG_(get_out_file)()
{
    CLG_(init_dumps)();
    return out_file;
}

Char* CLG_(get_out_directory)()
{
    CLG_(init_dumps)();
    return out_directory;
}

/*------------------------------------------------------------*/
/*--- Output file related stuff                            ---*/
/*------------------------------------------------------------*/

/* Boolean dumping array */
static Bool* dump_array = 0;
static Int   dump_array_size = 0;
static Bool* obj_dumped = 0;
static Bool* file_dumped = 0;
static Bool* fn_dumped = 0;
static Bool* cxt_dumped = 0;

static
void reset_dump_array(void)
{
    int i;

    CLG_ASSERT(dump_array != 0);

    for(i=0;i<dump_array_size;i++)
	dump_array[i] = False;
}

static
void init_dump_array(void)
{
    dump_array_size = CLG_(stat).distinct_objs +
      CLG_(stat).distinct_files +
      CLG_(stat).distinct_fns +
      CLG_(stat).context_counter;
    CLG_ASSERT(dump_array == 0);
    dump_array = (Bool*) CLG_MALLOC("cl.dump.ida.1",
                                    dump_array_size * sizeof(Bool));
    obj_dumped  = dump_array;
    file_dumped = obj_dumped + CLG_(stat).distinct_objs;
    fn_dumped   = file_dumped + CLG_(stat).distinct_files;
    cxt_dumped  = fn_dumped + CLG_(stat).distinct_fns;

    reset_dump_array();

    CLG_DEBUG(1, "  init_dump_array: size %d\n", dump_array_size);
}

static __inline__
void free_dump_array(void)
{
    CLG_ASSERT(dump_array != 0);
    VG_(free)(dump_array);

    dump_array = 0;
    obj_dumped = 0;
    file_dumped = 0;
    fn_dumped = 0;
    cxt_dumped = 0;
}


/* Initialize to an invalid position */
static __inline__
void init_fpos(FnPos* p)
 {
    p->file = 0;
    p->fn = 0;
    p->obj = 0;
    p->cxt = 0;
    p->rec_index = 0;
}


#if 0
static __inline__
static void my_fwrite(Int fd, Char* buf, Int len)
{
	VG_(write)(fd, (void*)buf, len);
}
#else

#define FWRITE_BUFSIZE 32000
#define FWRITE_THROUGH 10000
static Char fwrite_buf[FWRITE_BUFSIZE];
static Int fwrite_pos;
static Int fwrite_fd = -1;

static __inline__
void fwrite_flush(void)
{
    if ((fwrite_fd>=0) && (fwrite_pos>0))
	VG_(write)(fwrite_fd, (void*)fwrite_buf, fwrite_pos);
    fwrite_pos = 0;
}

static void my_fwrite(Int fd, Char* buf, Int len)
{
    if (fwrite_fd != fd) {
	fwrite_flush();
	fwrite_fd = fd;
    }
    if (len > FWRITE_THROUGH) {
	fwrite_flush();
	VG_(write)(fd, (void*)buf, len);
	return;
    }
    if (FWRITE_BUFSIZE - fwrite_pos <= len) fwrite_flush();
    VG_(strncpy)(fwrite_buf + fwrite_pos, buf, len);
    fwrite_pos += len;
}
#endif


static void print_obj(Char* buf, obj_node* obj)
{
    //int n;

    if (CLG_(clo).compress_strings) {
	CLG_ASSERT(obj_dumped != 0);
	if (obj_dumped[obj->number])
	    /*n =*/ VG_(sprintf)(buf, "(%d)\n", obj->number);
	else {
	    /*n =*/ VG_(sprintf)(buf, "(%d) %s\n",
			     obj->number, obj->name);
	}
    }
    else
	/*n =*/ VG_(sprintf)(buf, "%s\n", obj->name);

#if 0
    /* add mapping parameters the first time a object is dumped
     * format: mp=0xSTART SIZE 0xOFFSET */
    if (!obj_dumped[obj->number]) {
	obj_dumped[obj->number];
	VG_(sprintf)(buf+n, "mp=%p %p %p\n",
		     pos->obj->start, pos->obj->size, pos->obj->offset);
    }
#else
    obj_dumped[obj->number] = True;
#endif
}

static void print_file(Char* buf, file_node* file)
{
    if (CLG_(clo).compress_strings) {
	CLG_ASSERT(file_dumped != 0);
	if (file_dumped[file->number])
	    VG_(sprintf)(buf, "(%d)\n", file->number);
	else {
	    VG_(sprintf)(buf, "(%d) %s\n",
			 file->number, file->name);
	    file_dumped[file->number] = True;
	}
    }
    else
	VG_(sprintf)(buf, "%s\n", file->name);
}

/*
 * tag can be "fn", "cfn", "jfn"
 */
static void print_fn(Int fd, Char* buf, Char* tag, fn_node* fn)
{
    int p;
    p = VG_(sprintf)(buf, "%s=",tag);
    if (CLG_(clo).compress_strings) {
	CLG_ASSERT(fn_dumped != 0);
	if (fn_dumped[fn->number])
	    p += VG_(sprintf)(buf+p, "(%d)\n", fn->number);
	else {
	    p += VG_(sprintf)(buf+p, "(%d) %s\n",
			      fn->number, fn->name);
	    fn_dumped[fn->number] = True;
	}
    }
    else
	p += VG_(sprintf)(buf+p, "%s\n", fn->name);

    my_fwrite(fd, buf, p);
}

static void print_mangled_fn(Int fd, Char* buf, Char* tag, 
			     Context* cxt, int rec_index)
{
    int p, i;

    if (CLG_(clo).compress_strings && CLG_(clo).compress_mangled) {

	int n;
	Context* last;

	CLG_ASSERT(cxt_dumped != 0);
	if (cxt_dumped[cxt->base_number+rec_index]) {
	    p = VG_(sprintf)(buf, "%s=(%d)\n",
			     tag, cxt->base_number + rec_index);
	    my_fwrite(fd, buf, p);
	    return;
	}

	last = 0;
	/* make sure that for all context parts compressed data is written */
	for(i=cxt->size;i>0;i--) {
	    CLG_ASSERT(cxt->fn[i-1]->pure_cxt != 0);
	    n = cxt->fn[i-1]->pure_cxt->base_number;
	    if (cxt_dumped[n]) continue;
	    p = VG_(sprintf)(buf, "%s=(%d) %s\n",
			     tag, n, cxt->fn[i-1]->name);
	    my_fwrite(fd, buf, p);

	    cxt_dumped[n] = True;
	    last = cxt->fn[i-1]->pure_cxt;
	}
	/* If the last context was the context to print, we are finished */
	if ((last == cxt) && (rec_index == 0)) return;

	p = VG_(sprintf)(buf, "%s=(%d) (%d)", tag,
			 cxt->base_number + rec_index,
			 cxt->fn[0]->pure_cxt->base_number);
	if (rec_index >0)
	    p += VG_(sprintf)(buf+p, "'%d", rec_index +1);
	for(i=1;i<cxt->size;i++)
	    p += VG_(sprintf)(buf+p, "'(%d)", 
			      cxt->fn[i]->pure_cxt->base_number);
	p += VG_(sprintf)(buf+p, "\n");
	my_fwrite(fd, buf, p);

	cxt_dumped[cxt->base_number+rec_index] = True;
	return;
    }


    p = VG_(sprintf)(buf, "%s=", tag);
    if (CLG_(clo).compress_strings) {
	CLG_ASSERT(cxt_dumped != 0);
	if (cxt_dumped[cxt->base_number+rec_index]) {
	    p += VG_(sprintf)(buf+p, "(%d)\n", cxt->base_number + rec_index);
	    my_fwrite(fd, buf, p);
	    return;
	}
	else {
	    p += VG_(sprintf)(buf+p, "(%d) ", cxt->base_number + rec_index);
	    cxt_dumped[cxt->base_number+rec_index] = True;
	}
    }

    p += VG_(sprintf)(buf+p, "%s", cxt->fn[0]->name);
    if (rec_index >0)
	p += VG_(sprintf)(buf+p, "'%d", rec_index +1);
    for(i=1;i<cxt->size;i++)
	p += VG_(sprintf)(buf+p, "'%s", cxt->fn[i]->name);

    p += VG_(sprintf)(buf+p, "\n");
    my_fwrite(fd, buf, p);
}



/**
 * Print function position of the BBCC, but only print info differing to
 * the <last> position, update <last>
 * Return True if something changes.
 */
static Bool print_fn_pos(int fd, FnPos* last, BBCC* bbcc)
{
    Bool res = False;

    CLG_ASSERT(bbcc && bbcc->cxt);

    CLG_DEBUGIF(3) {
	CLG_DEBUG(2, "+ print_fn_pos: ");
	CLG_(print_cxt)(16, bbcc->cxt, bbcc->rec_index);
    }

    if (!CLG_(clo).mangle_names) {
	if (last->rec_index != bbcc->rec_index) {
	    VG_(sprintf)(outbuf, "rec=%d\n\n", bbcc->rec_index);
	    my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));
	    last->rec_index = bbcc->rec_index;
	    last->cxt = 0; /* reprint context */
	    res = True;
	}
	
	if (last->cxt != bbcc->cxt) {
	    fn_node* last_from = (last->cxt && last->cxt->size >1) ?
				 last->cxt->fn[1] : 0;
	    fn_node* curr_from = (bbcc->cxt->size >1) ?
				 bbcc->cxt->fn[1] : 0;
	    if (curr_from == 0) {
		if (last_from != 0) {
		    /* switch back to no context */
		    VG_(sprintf)(outbuf, "frfn=(spontaneous)\n");
		    my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));
		    res = True;
		}
	    }
	    else if (last_from != curr_from) {
		print_fn(fd,outbuf,"frfn", curr_from);
		res = True;
	    }
	    last->cxt = bbcc->cxt;
	}
    }

    if (last->obj != bbcc->cxt->fn[0]->file->obj) {
	VG_(sprintf)(outbuf, "ob=");
	print_obj(outbuf+3, bbcc->cxt->fn[0]->file->obj);
	my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));
	last->obj = bbcc->cxt->fn[0]->file->obj;
	res = True;
    }

    if (last->file != bbcc->cxt->fn[0]->file) {
	VG_(sprintf)(outbuf, "fl=");
	print_file(outbuf+3, bbcc->cxt->fn[0]->file);
	my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));
	last->file = bbcc->cxt->fn[0]->file;
	res = True;
    }

    if (!CLG_(clo).mangle_names) {
	if (last->fn != bbcc->cxt->fn[0]) {
	    print_fn(fd,outbuf, "fn", bbcc->cxt->fn[0]);
	    last->fn = bbcc->cxt->fn[0];
	    res = True;
	}
    }
    else {
	/* Print mangled name if context or rec_index changes */
	if ((last->rec_index != bbcc->rec_index) ||
	    (last->cxt != bbcc->cxt)) {

	    print_mangled_fn(fd, outbuf, "fn", bbcc->cxt, bbcc->rec_index);
	    last->fn = bbcc->cxt->fn[0];
	    last->rec_index = bbcc->rec_index;
	    res = True;
	}
    }

    last->cxt = bbcc->cxt;

    CLG_DEBUG(2, "- print_fn_pos: %s\n", res ? "changed" : "");
    
    return res;
}

/* the debug lookup cache is useful if BBCC for same BB are
 * dumped directly in a row. This is a direct mapped cache.
 */
#define DEBUG_CACHE_SIZE 1777

static Addr       debug_cache_addr[DEBUG_CACHE_SIZE];
static file_node* debug_cache_file[DEBUG_CACHE_SIZE];
static int        debug_cache_line[DEBUG_CACHE_SIZE];
static Bool       debug_cache_info[DEBUG_CACHE_SIZE];

static __inline__
void init_debug_cache(void)
{
    int i;
    for(i=0;i<DEBUG_CACHE_SIZE;i++) {
	debug_cache_addr[i] = 0;
	debug_cache_file[i] = 0;
	debug_cache_line[i] = 0;
	debug_cache_info[i] = 0;
    }
}

static /* __inline__ */
Bool get_debug_pos(BBCC* bbcc, Addr addr, AddrPos* p)
{
    Char file[FILENAME_LEN];
    Char dir[FILENAME_LEN];
    Bool found_file_line, found_dirname;

    int cachepos = addr % DEBUG_CACHE_SIZE;
    
    if (debug_cache_addr[cachepos] == addr) {
	p->line = debug_cache_line[cachepos];
	p->file = debug_cache_file[cachepos];
	found_file_line = debug_cache_info[cachepos];
    }
    else {
	found_file_line = VG_(get_filename_linenum)(addr,
						    file, FILENAME_LEN,
						    dir, FILENAME_LEN,
						    &found_dirname,
						    &(p->line));
	if (!found_file_line) {
	    VG_(strcpy)(file, "???");
	    p->line = 0;
	}
	if (found_dirname) {
	    // +1 for the '/'.
	    CLG_ASSERT(VG_(strlen)(dir) + VG_(strlen)(file) + 1 < FILENAME_LEN);
	    VG_(strcat)(dir, "/");     // Append '/'
	    VG_(strcat)(dir, file);    // Append file to dir
	    VG_(strcpy)(file, dir);    // Move dir+file to file
	}
	p->file    = CLG_(get_file_node)(bbcc->bb->obj, file);

	debug_cache_info[cachepos] = found_file_line;
	debug_cache_addr[cachepos] = addr;
	debug_cache_line[cachepos] = p->line;
	debug_cache_file[cachepos] = p->file;
    }

    /* Address offset from bbcc start address */
    p->addr = addr - bbcc->bb->obj->offset;
    p->bb_addr = bbcc->bb->offset;

    CLG_DEBUG(3, "  get_debug_pos(%#lx): BB %#lx, fn '%s', file '%s', line %u\n",
	     addr, bb_addr(bbcc->bb), bbcc->cxt->fn[0]->name,
	     p->file->name, p->line);

    return found_file_line;
}


/* copy file position and init cost */
static void init_apos(AddrPos* p, Addr addr, Addr bbaddr, file_node* file)
{
    p->addr    = addr;
    p->bb_addr = bbaddr;
    p->file    = file;
    p->line    = 0;
}

static void copy_apos(AddrPos* dst, AddrPos* src)
{
    dst->addr    = src->addr;
    dst->bb_addr = src->bb_addr;
    dst->file    = src->file;
    dst->line    = src->line;
}   

/* copy file position and init cost */
static void init_fcost(AddrCost* c, Addr addr, Addr bbaddr, file_node* file)
{
    init_apos( &(c->p), addr, bbaddr, file);
    /* FIXME: This is a memory leak as a AddrCost is inited multiple times */
    c->cost = CLG_(get_eventset_cost)( CLG_(sets).full );
    CLG_(init_cost)( CLG_(sets).full, c->cost );
}


/**
 * print position change inside of a BB (last -> curr)
 * this doesn't update last to curr!
 */
static void fprint_apos(Int fd, AddrPos* curr, AddrPos* last, file_node* func_file)
{
    CLG_ASSERT(curr->file != 0);
    CLG_DEBUG(2, "    print_apos(file '%s', line %d, bb %#lx, addr %#lx) fnFile '%s'\n",
	     curr->file->name, curr->line, curr->bb_addr, curr->addr,
	     func_file->name);

    if (curr->file != last->file) {

	/* if we switch back to orig file, use fe=... */
	if (curr->file == func_file)
	    VG_(sprintf)(outbuf, "fe=");
	else
	    VG_(sprintf)(outbuf, "fi=");
	print_file(outbuf+3, curr->file);
	my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));
    }

    if (CLG_(clo).dump_bbs) {
	if (curr->line != last->line) {
	    VG_(sprintf)(outbuf, "ln=%d\n", curr->line);
	    my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));
	}
    }
}



/**
 * Print a position.
 * This prints out differences if allowed
 *
 * This doesn't set last to curr afterwards!
 */
static
void fprint_pos(Int fd, AddrPos* curr, AddrPos* last)
{
    if (0) //CLG_(clo).dump_bbs)
	VG_(sprintf)(outbuf, "%lu ", curr->addr - curr->bb_addr);
    else {
	int p = 0;
	if (CLG_(clo).dump_instr) {
	    int diff = curr->addr - last->addr;
	    if ( CLG_(clo).compress_pos && (last->addr >0) && 
		 (diff > -100) && (diff < 100)) {
		if (diff >0)
		    p = VG_(sprintf)(outbuf, "+%d ", diff);
		else if (diff==0)
		    p = VG_(sprintf)(outbuf, "* ");
	        else
		    p = VG_(sprintf)(outbuf, "%d ", diff);
	    }
	    else
		p = VG_(sprintf)(outbuf, "%#lx ", curr->addr);
	}

	if (CLG_(clo).dump_bb) {
	    int diff = curr->bb_addr - last->bb_addr;
	    if ( CLG_(clo).compress_pos && (last->bb_addr >0) && 
		 (diff > -100) && (diff < 100)) {
		if (diff >0)
		    p += VG_(sprintf)(outbuf+p, "+%d ", diff);
		else if (diff==0)
		    p += VG_(sprintf)(outbuf+p, "* ");
	        else
		    p += VG_(sprintf)(outbuf+p, "%d ", diff);
	    }
	    else
		p += VG_(sprintf)(outbuf+p, "%#lx ", curr->bb_addr);
	}

	if (CLG_(clo).dump_line) {
	    int diff = curr->line - last->line;
	    if ( CLG_(clo).compress_pos && (last->line >0) && 
		 (diff > -100) && (diff < 100)) {

		if (diff >0)
		    VG_(sprintf)(outbuf+p, "+%d ", diff);
		else if (diff==0)
		    VG_(sprintf)(outbuf+p, "* ");
	        else
		    VG_(sprintf)(outbuf+p, "%d ", diff);
	    }
	    else
		VG_(sprintf)(outbuf+p, "%u ", curr->line);
	}
    }
    my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));
}


/**
 * Print events.
 */

static
void fprint_cost(int fd, EventMapping* es, ULong* cost)
{
  int p = CLG_(sprint_mappingcost)(outbuf, es, cost);
  VG_(sprintf)(outbuf+p, "\n");
  my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));
  return;
}



/* Write the cost of a source line; only that parts of the source
 * position are written that changed relative to last written position.
 * funcPos is the source position of the first line of actual function.
 * Something is written only if cost != 0; returns True in this case.
 */
static void fprint_fcost(Int fd, AddrCost* c, AddrPos* last)
{
  CLG_DEBUGIF(3) {
    CLG_DEBUG(2, "   print_fcost(file '%s', line %d, bb %#lx, addr %#lx):\n",
	     c->p.file->name, c->p.line, c->p.bb_addr, c->p.addr);
    CLG_(print_cost)(-5, CLG_(sets).full, c->cost);
  }
    
  fprint_pos(fd, &(c->p), last);
  copy_apos( last, &(c->p) ); /* update last to current position */

  fprint_cost(fd, CLG_(dumpmap), c->cost);

  /* add cost to total */
  CLG_(add_and_zero_cost)( CLG_(sets).full, dump_total_cost, c->cost );
}


/* Write out the calls from jcc (at pos)
 */
static void fprint_jcc(Int fd, jCC* jcc, AddrPos* curr, AddrPos* last, ULong ecounter)
{
    static AddrPos target;
    file_node* file;
    obj_node*  obj;

    CLG_DEBUGIF(2) {
      CLG_DEBUG(2, "   fprint_jcc (jkind %d)\n", jcc->jmpkind);
      CLG_(print_jcc)(-10, jcc);
    }

    CLG_ASSERT(jcc->to !=0);
    CLG_ASSERT(jcc->from !=0);
    
    if (!get_debug_pos(jcc->to, bb_addr(jcc->to->bb), &target)) {
	/* if we don't have debug info, don't switch to file "???" */
	target.file = last->file;
    }

    if ((jcc->jmpkind == JmpCond) || (jcc->jmpkind == Ijk_Boring)) {
	    
      /* this is a JCC for a followed conditional or boring jump. */
      CLG_ASSERT(CLG_(is_zero_cost)( CLG_(sets).full, jcc->cost));
	
      /* objects among jumps should be the same.
       * Otherwise this jump would have been changed to a call
       *  (see setup_bbcc)
       */
      CLG_ASSERT(jcc->from->bb->obj == jcc->to->bb->obj);

	/* only print if target position info is usefull */
	if (!CLG_(clo).dump_instr && !CLG_(clo).dump_bb && target.line==0) {
	  jcc->call_counter = 0;
	  return;
	}

	/* Different files/functions are possible e.g. with longjmp's
	 * which change the stack, and thus context
	 */
	if (last->file != target.file) {
	    VG_(sprintf)(outbuf, "jfi=");
	    print_file(outbuf+4, target.file);
	    my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));
	}
	
	if (jcc->from->cxt != jcc->to->cxt) {
	    if (CLG_(clo).mangle_names)
		print_mangled_fn(fd, outbuf, "jfn",
				 jcc->to->cxt, jcc->to->rec_index);
	    else
		print_fn(fd, outbuf, "jfn", jcc->to->cxt->fn[0]);
	}
	    
	if (jcc->jmpkind == JmpCond) {
	    /* format: jcnd=<followed>/<executions> <target> */
	    VG_(sprintf)(outbuf, "jcnd=%llu/%llu ",
			 jcc->call_counter, ecounter);
	}
	else {
	    /* format: jump=<jump count> <target> */
	    VG_(sprintf)(outbuf, "jump=%llu ",
			 jcc->call_counter);
	}
	my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));
		
	fprint_pos(fd, &target, last);
	my_fwrite(fd, "\n", 1);
	fprint_pos(fd, curr, last);
	my_fwrite(fd, "\n", 1);

	jcc->call_counter = 0;
	return;
    }

    file = jcc->to->cxt->fn[0]->file;
    obj  = jcc->to->bb->obj;
    
    /* object of called position different to object of this function?*/
    if (jcc->from->cxt->fn[0]->file->obj != obj) {
	VG_(sprintf)(outbuf, "cob=");
	print_obj(outbuf+4, obj);
	my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));
    }

    /* file of called position different to current file? */
    if (last->file != file) {
	VG_(sprintf)(outbuf, "cfi=");
	print_file(outbuf+4, file);
	my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));
    }

    if (CLG_(clo).mangle_names)
	print_mangled_fn(fd, outbuf, "cfn", jcc->to->cxt, jcc->to->rec_index);
    else
	print_fn(fd, outbuf, "cfn", jcc->to->cxt->fn[0]);

    if (!CLG_(is_zero_cost)( CLG_(sets).full, jcc->cost)) {
      VG_(sprintf)(outbuf, "calls=%llu ", 
		   jcc->call_counter);
	my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));

	fprint_pos(fd, &target, last);
	my_fwrite(fd, "\n", 1);	
	fprint_pos(fd, curr, last);
	fprint_cost(fd, CLG_(dumpmap), jcc->cost);

	CLG_(init_cost)( CLG_(sets).full, jcc->cost );

	jcc->call_counter = 0;
    }
}



/* Cost summation of functions.We use alternately ccSum[0/1], thus
 * ssSum[currSum] for recently read lines with same line number.
 */
static AddrCost ccSum[2];
static int currSum;

/*
 * Print all costs of a BBCC:
 * - FCCs of instructions
 * - JCCs of the unique jump of this BB
 * returns True if something was written 
 */
static Bool fprint_bbcc(Int fd, BBCC* bbcc, AddrPos* last)
{
  InstrInfo* instr_info;
  ULong ecounter;
  Bool something_written = False;
  jCC* jcc;
  AddrCost *currCost, *newCost;
  Int jcc_count = 0, instr, i, jmp;
  BB* bb = bbcc->bb;

  CLG_ASSERT(bbcc->cxt != 0);
  CLG_DEBUGIF(1) {
    VG_(printf)("+ fprint_bbcc (Instr %d): ", bb->instr_count);
    CLG_(print_bbcc)(15, bbcc);
  }

  CLG_ASSERT(currSum == 0 || currSum == 1);
  currCost = &(ccSum[currSum]);
  newCost  = &(ccSum[1-currSum]);

  ecounter = bbcc->ecounter_sum;
  jmp = 0;
  instr_info = &(bb->instr[0]);
  for(instr=0; instr<bb->instr_count; instr++, instr_info++) {

    /* get debug info of current instruction address and dump cost
     * if CLG_(clo).dump_bbs or file/line has changed
     */
    if (!get_debug_pos(bbcc, bb_addr(bb) + instr_info->instr_offset, 
		       &(newCost->p))) {
      /* if we don't have debug info, don't switch to file "???" */
      newCost->p.file = bbcc->cxt->fn[0]->file;
    }

    if (CLG_(clo).dump_bbs || CLG_(clo).dump_instr ||
	(newCost->p.line != currCost->p.line) ||
	(newCost->p.file != currCost->p.file)) {
      
      if (!CLG_(is_zero_cost)( CLG_(sets).full, currCost->cost )) {
	something_written = True;
	
	fprint_apos(fd, &(currCost->p), last, bbcc->cxt->fn[0]->file);
	fprint_fcost(fd, currCost, last);
      }
	   
      /* switch buffers */
      currSum = 1 - currSum;
      currCost = &(ccSum[currSum]);
      newCost  = &(ccSum[1-currSum]);
    }
       
    /* add line cost to current cost sum */
    (*CLG_(cachesim).add_icost)(currCost->cost, bbcc, instr_info, ecounter);

    /* print jcc's if there are: only jumps */
    if (bb->jmp[jmp].instr == instr) {
	jcc_count=0;
	for(jcc=bbcc->jmp[jmp].jcc_list; jcc; jcc=jcc->next_from)
	    if (((jcc->jmpkind != Ijk_Call) && (jcc->call_counter >0)) ||
		(!CLG_(is_zero_cost)( CLG_(sets).full, jcc->cost )))
	      jcc_count++;

	if (jcc_count>0) {    
	    if (!CLG_(is_zero_cost)( CLG_(sets).full, currCost->cost )) {
		/* no need to switch buffers, as position is the same */
		fprint_apos(fd, &(currCost->p), last, bbcc->cxt->fn[0]->file);
		fprint_fcost(fd, currCost, last);
	    }
	    get_debug_pos(bbcc, bb_addr(bb)+instr_info->instr_offset, &(currCost->p));
	    fprint_apos(fd, &(currCost->p), last, bbcc->cxt->fn[0]->file);
	    something_written = True;
	    for(jcc=bbcc->jmp[jmp].jcc_list; jcc; jcc=jcc->next_from) {
		if (((jcc->jmpkind != Ijk_Call) && (jcc->call_counter >0)) ||
		    (!CLG_(is_zero_cost)( CLG_(sets).full, jcc->cost )))
		    fprint_jcc(fd, jcc, &(currCost->p), last, ecounter);
	    }
	}
    }

    /* update execution counter */
    if (jmp < bb->cjmp_count)
	if (bb->jmp[jmp].instr == instr) {
	    ecounter -= bbcc->jmp[jmp].ecounter;
	    jmp++;
	}
  }
  
  /* jCCs at end? If yes, dump cumulated line info first */
  jcc_count = 0;
  for(jcc=bbcc->jmp[jmp].jcc_list; jcc; jcc=jcc->next_from) {
      /* yes, if JCC only counts jmp arcs or cost >0 */
      if ( ((jcc->jmpkind != Ijk_Call) && (jcc->call_counter >0)) ||
	   (!CLG_(is_zero_cost)( CLG_(sets).full, jcc->cost )))
	  jcc_count++;
  }
  
  if ( (bbcc->skipped &&
	!CLG_(is_zero_cost)(CLG_(sets).full, bbcc->skipped)) || 
       (jcc_count>0) ) {
    
    if (!CLG_(is_zero_cost)( CLG_(sets).full, currCost->cost )) {
      /* no need to switch buffers, as position is the same */
      fprint_apos(fd, &(currCost->p), last, bbcc->cxt->fn[0]->file);
      fprint_fcost(fd, currCost, last);
    }
    
    get_debug_pos(bbcc, bb_jmpaddr(bb), &(currCost->p));
    fprint_apos(fd, &(currCost->p), last, bbcc->cxt->fn[0]->file);
    something_written = True;
    
    /* first, print skipped costs for calls */
    if (bbcc->skipped && !CLG_(is_zero_cost)( CLG_(sets).full,
					     bbcc->skipped )) {
      CLG_(add_and_zero_cost)( CLG_(sets).full,
			      currCost->cost, bbcc->skipped );
#if 0
      VG_(sprintf)(outbuf, "# Skipped\n");
      my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));
#endif
      fprint_fcost(fd, currCost, last);
    }

    if (jcc_count > 0)
	for(jcc=bbcc->jmp[jmp].jcc_list; jcc; jcc=jcc->next_from) {
	    CLG_ASSERT(jcc->jmp == jmp);
	    if ( ((jcc->jmpkind != Ijk_Call) && (jcc->call_counter >0)) ||
		 (!CLG_(is_zero_cost)( CLG_(sets).full, jcc->cost )))
	  
		fprint_jcc(fd, jcc, &(currCost->p), last, ecounter);
	}
  }

  if (CLG_(clo).dump_bbs || CLG_(clo).dump_bb) {
    if (!CLG_(is_zero_cost)( CLG_(sets).full, currCost->cost )) {
      something_written = True;
      
      fprint_apos(fd, &(currCost->p), last, bbcc->cxt->fn[0]->file);
      fprint_fcost(fd, currCost, last);
    }
    if (CLG_(clo).dump_bbs) my_fwrite(fd, (void*)"\n", 1);
    
    /* when every cost was immediatly written, we must have done so,
     * as this function is only called when there's cost in a BBCC
     */
    CLG_ASSERT(something_written);
  }
  
  bbcc->ecounter_sum = 0;
  for(i=0; i<=bbcc->bb->cjmp_count; i++)
    bbcc->jmp[i].ecounter = 0;
  bbcc->ret_counter = 0;
  
  CLG_DEBUG(1, "- fprint_bbcc: JCCs %d\n", jcc_count);
  
  return something_written;
}

/* order by
 *  recursion,
 *  from->bb->obj, from->bb->fn
 *  obj, fn[0]->file, fn
 *  address
 */
static int my_cmp(BBCC** pbbcc1, BBCC** pbbcc2)
{
#if 0
    return (*pbbcc1)->bb->offset - (*pbbcc2)->bb->offset;
#else
    BBCC *bbcc1 = *pbbcc1;
    BBCC *bbcc2 = *pbbcc2;
    Context* cxt1 = bbcc1->cxt;
    Context* cxt2 = bbcc2->cxt;
    int off = 1;

    if (cxt1->fn[0]->file->obj != cxt2->fn[0]->file->obj)
	return cxt1->fn[0]->file->obj - cxt2->fn[0]->file->obj;

    if (cxt1->fn[0]->file != cxt2->fn[0]->file)
	return cxt1->fn[0]->file - cxt2->fn[0]->file;

    if (cxt1->fn[0] != cxt2->fn[0])
	return cxt1->fn[0] - cxt2->fn[0];

    if (bbcc1->rec_index != bbcc2->rec_index)
	return bbcc1->rec_index - bbcc2->rec_index;

    while((off < cxt1->size) && (off < cxt2->size)) {
	fn_node* ffn1 = cxt1->fn[off];
	fn_node* ffn2 = cxt2->fn[off];
	if (ffn1->file->obj != ffn2->file->obj)
	    return ffn1->file->obj - ffn2->file->obj;
	if (ffn1 != ffn2)
	    return ffn1 - ffn2;
	off++;
    }
    if      (cxt1->size > cxt2->size) return 1;
    else if (cxt1->size < cxt2->size) return -1;

    return bbcc1->bb->offset - bbcc2->bb->offset;
#endif
}





/* modified version of:
 *
 * qsort -- qsort interface implemented by faster quicksort.
 * J. L. Bentley and M. D. McIlroy, SPE 23 (1993) 1249-1265.
 * Copyright 1993, John Wiley.
*/

static __inline__
void swapfunc(BBCC** a, BBCC** b, int n)
{
    while(n>0) {
	BBCC* t = *a; *a = *b; *b = t;
	a++, b++;
	n--;
    }
}

static __inline__
void swap(BBCC** a, BBCC** b)
{
    BBCC* t;
    t = *a; *a = *b; *b = t;
}

#define min(x, y) ((x)<=(y) ? (x) : (y))

static
BBCC** med3(BBCC **a, BBCC **b, BBCC **c, int (*cmp)(BBCC**,BBCC**))
{	return cmp(a, b) < 0 ?
		  (cmp(b, c) < 0 ? b : cmp(a, c) < 0 ? c : a)
		: (cmp(b, c) > 0 ? b : cmp(a, c) > 0 ? c : a);
}

static BBCC** qsort_start = 0;

static void qsort(BBCC **a, int n, int (*cmp)(BBCC**,BBCC**))
{
	BBCC **pa, **pb, **pc, **pd, **pl, **pm, **pn, **pv;
	int s, r;
	BBCC* v;

	CLG_DEBUG(8, "  qsort(%ld,%ld)\n", a-qsort_start + 0L, n + 0L);

	if (n < 7) {	 /* Insertion sort on smallest arrays */
		for (pm = a+1; pm < a+n; pm++)
			for (pl = pm; pl > a && cmp(pl-1, pl) > 0; pl --)
				swap(pl, pl-1);

		CLG_DEBUGIF(8) {
		    for (pm = a; pm < a+n; pm++) {
			VG_(printf)("   %3ld BB %#lx, ",
                                    pm - qsort_start + 0L,
				    bb_addr((*pm)->bb));      
			CLG_(print_cxt)(9, (*pm)->cxt, (*pm)->rec_index);
		    }
		}
		return;
	}
	pm = a + n/2;    /* Small arrays, middle element */
	if (n > 7) {
		pl = a;
		pn = a + (n-1);
		if (n > 40) {    /* Big arrays, pseudomedian of 9 */
			s = n/8;
			pl = med3(pl, pl+s, pl+2*s, cmp);
			pm = med3(pm-s, pm, pm+s, cmp);
			pn = med3(pn-2*s, pn-s, pn, cmp);
		}
		pm = med3(pl, pm, pn, cmp); /* Mid-size, med of 3 */
	}


	v = *pm;
	pv = &v;
	pa = pb = a;
	pc = pd = a + (n-1);
	for (;;) {
		while ((pb <= pc) && ((r=cmp(pb, pv)) <= 0)) {
		    if (r==0) {
			/* same as pivot, to start */
			swap(pa,pb); pa++; 
		    }
		    pb ++;
		}
		while ((pb <= pc) && ((r=cmp(pc, pv)) >= 0)) {
		    if (r==0) {
			/* same as pivot, to end */
			swap(pc,pd); pd--; 
		    }
		    pc --;
		}
		if (pb > pc) { break; }
		swap(pb, pc);
		pb ++;
		pc --;
	}
	pb--;
	pc++;

	/* put pivot from start into middle */
	if ((s = pa-a)>0) { for(r=0;r<s;r++) swap(a+r, pb+1-s+r); }
	/* put pivot from end into middle */
	if ((s = a+n-1-pd)>0) { for(r=0;r<s;r++) swap(pc+r, a+n-s+r); }	    

	CLG_DEBUGIF(8) {
	  VG_(printf)("   PV BB %#lx, ", bb_addr((*pv)->bb));
	    CLG_(print_cxt)(9, (*pv)->cxt, (*pv)->rec_index);

	    s = pb-pa+1;
	    VG_(printf)("    Lower %ld - %ld:\n",
                        a-qsort_start + 0L,
                        a+s-1-qsort_start + 0L);
	    for (r=0;r<s;r++) {
		pm = a+r;
		VG_(printf)("     %3ld BB %#lx, ",
			    pm-qsort_start + 0L,
                            bb_addr((*pm)->bb));
		CLG_(print_cxt)(9, (*pm)->cxt, (*pm)->rec_index);
	    }

	    s = pd-pc+1;
	    VG_(printf)("    Upper %ld - %ld:\n",
			a+n-s-qsort_start + 0L,
                        a+n-1-qsort_start + 0L);
	    for (r=0;r<s;r++) {
		pm = a+n-s+r;
		VG_(printf)("     %3ld BB %#lx, ",
			    pm-qsort_start + 0L,
                            bb_addr((*pm)->bb));
		CLG_(print_cxt)(9, (*pm)->cxt, (*pm)->rec_index);
	    }
	}

	if ((s = pb+1-pa) > 1) qsort(a,     s, cmp);
	if ((s = pd+1-pc) > 1) qsort(a+n-s, s, cmp);
}


/* Helpers for prepare_dump */

static Int    prepare_count;
static BBCC** prepare_ptr;


static void hash_addCount(BBCC* bbcc)
{
  if ((bbcc->ecounter_sum > 0) || (bbcc->ret_counter>0))
    prepare_count++;
}

static void hash_addPtr(BBCC* bbcc)
{
  if ((bbcc->ecounter_sum == 0) &&
      (bbcc->ret_counter == 0)) return;

  *prepare_ptr = bbcc;
  prepare_ptr++;
}


static void cs_addCount(thread_info* ti)
{
  Int i;
  BBCC* bbcc;

  /* add BBCCs with active call in call stack of current thread.
   * update cost sums for active calls
   */
      
  for(i = 0; i < CLG_(current_call_stack).sp; i++) {
    call_entry* e = &(CLG_(current_call_stack).entry[i]);
    if (e->jcc == 0) continue;
    
    CLG_(add_diff_cost_lz)( CLG_(sets).full, &(e->jcc->cost),
			   e->enter_cost, CLG_(current_state).cost);
    bbcc = e->jcc->from;

    CLG_DEBUG(1, " [%2d] (tid %d), added active: %s\n",
	     i,CLG_(current_tid),bbcc->cxt->fn[0]->name);
    
    if (bbcc->ecounter_sum>0 || bbcc->ret_counter>0) {
      /* already counted */
      continue;
    }
    prepare_count++;
  }
}

static void cs_addPtr(thread_info* ti)
{
  Int i;
  BBCC* bbcc;

  /* add BBCCs with active call in call stack of current thread.
   * update cost sums for active calls
   */
      
  for(i = 0; i < CLG_(current_call_stack).sp; i++) {
    call_entry* e = &(CLG_(current_call_stack).entry[i]);
    if (e->jcc == 0) continue;

    bbcc = e->jcc->from;
    
    if (bbcc->ecounter_sum>0 || bbcc->ret_counter>0) {
      /* already counted */
      continue;
    }

    *prepare_ptr = bbcc;
    prepare_ptr++;
  }
}


/**
 * Put all BBCCs with costs into a sorted array.
 * The returned arrays ends with a null pointer. 
 * Must be freed after dumping.
 */
static
BBCC** prepare_dump(void)
{
    BBCC **array;

    prepare_count = 0;
    
    /* if we do not separate among threads, this gives all */
    /* count number of BBCCs with >0 executions */
    CLG_(forall_bbccs)(hash_addCount);

    /* even if we do not separate among threads,
     * call stacks are separated */
    if (CLG_(clo).separate_threads)
      cs_addCount(0);
    else
      CLG_(forall_threads)(cs_addCount);

    CLG_DEBUG(0, "prepare_dump: %d BBCCs\n", prepare_count);

    /* allocate bbcc array, insert BBCCs and sort */
    prepare_ptr = array =
      (BBCC**) CLG_MALLOC("cl.dump.pd.1",
                          (prepare_count+1) * sizeof(BBCC*));    

    CLG_(forall_bbccs)(hash_addPtr);

    if (CLG_(clo).separate_threads)
      cs_addPtr(0);
    else
      CLG_(forall_threads)(cs_addPtr);

    CLG_ASSERT(array + prepare_count == prepare_ptr);

    /* end mark */
    *prepare_ptr = 0;

    CLG_DEBUG(0,"             BBCCs inserted\n");

    qsort_start = array;
    qsort(array, prepare_count, my_cmp);

    CLG_DEBUG(0,"             BBCCs sorted\n");

    return array;
}




static void fprint_cost_ln(int fd, Char* prefix,
			   EventMapping* em, ULong* cost)
{
    int p;

    p = VG_(sprintf)(outbuf, "%s", prefix);
    p += CLG_(sprint_mappingcost)(outbuf + p, em, cost);
    VG_(sprintf)(outbuf + p, "\n");
    my_fwrite(fd, (void*)outbuf, VG_(strlen)(outbuf));
}

static ULong bbs_done = 0;
static Char* filename = 0;

static
void file_err(void)
{
   VG_(message)(Vg_UserMsg,
                "Error: can not open cache simulation output file `%s'\n",
                filename );
   VG_(exit)(1);
}

/**
 * Create a new dump file and write header.
 *
 * Naming: <CLG_(clo).filename_base>.<pid>[.<part>][-<tid>]
 *         <part> is skipped for final dump (trigger==0)
 *         <tid>  is skipped for thread 1 with CLG_(clo).separate_threads=no
 *
 * Returns the file descriptor, and -1 on error (no write permission)
 */
static int new_dumpfile(Char buf[BUF_LEN], int tid, Char* trigger)
{
    Bool appending = False;
    int i, fd;
    FullCost sum = 0;
    SysRes res;

    CLG_ASSERT(dumps_initialized);
    CLG_ASSERT(filename != 0);

    if (!CLG_(clo).combine_dumps) {
	i = VG_(sprintf)(filename, "%s", out_file);
    
	if (trigger)
	    i += VG_(sprintf)(filename+i, ".%d", out_counter);

	if (CLG_(clo).separate_threads)
	    VG_(sprintf)(filename+i, "-%02d", tid);

	res = VG_(open)(filename, VKI_O_WRONLY|VKI_O_TRUNC, 0);
    }
    else {
	VG_(sprintf)(filename, "%s", out_file);
        res = VG_(open)(filename, VKI_O_WRONLY|VKI_O_APPEND, 0);
	if (!sr_isError(res) && out_counter>1)
	    appending = True;
    }

    if (sr_isError(res)) {
	res = VG_(open)(filename, VKI_O_CREAT|VKI_O_WRONLY,
			VKI_S_IRUSR|VKI_S_IWUSR);
	if (sr_isError(res)) {
	    /* If the file can not be opened for whatever reason (conflict
	       between multiple supervised processes?), give up now. */
	    file_err();
	}
    }
    fd = (Int) sr_Res(res);

    CLG_DEBUG(2, "  new_dumpfile '%s'\n", filename);

    if (!appending)
	reset_dump_array();


    if (!appending) {
	/* version */
	VG_(sprintf)(buf, "version: 1\n");
	my_fwrite(fd, (void*)buf, VG_(strlen)(buf));

	/* creator */
	VG_(sprintf)(buf, "creator: callgrind-" VERSION "\n");
	my_fwrite(fd, (void*)buf, VG_(strlen)(buf));

	/* "pid:" line */
	VG_(sprintf)(buf, "pid: %d\n", VG_(getpid)());
	my_fwrite(fd, (void*)buf, VG_(strlen)(buf));

	/* "cmd:" line */
	VG_(strcpy)(buf, "cmd: ");
	my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
	my_fwrite(fd, (void*)cmdbuf, VG_(strlen)(cmdbuf));
    }

    VG_(sprintf)(buf, "\npart: %d\n", out_counter);
    my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
    if (CLG_(clo).separate_threads) {
	VG_(sprintf)(buf, "thread: %d\n", tid);
	my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
    }

    /* "desc:" lines */
    if (!appending) {
	my_fwrite(fd, "\n", 1);

#if 0
	/* Global options changing the tracing behaviour */
	VG_(sprintf)(buf, "\ndesc: Option: --skip-plt=%s\n",
		     CLG_(clo).skip_plt ? "yes" : "no");
	my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
	VG_(sprintf)(buf, "desc: Option: --collect-jumps=%s\n",
		     CLG_(clo).collect_jumps ? "yes" : "no");
	my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
	VG_(sprintf)(buf, "desc: Option: --separate-recs=%d\n",
		     CLG_(clo).separate_recursions);
	my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
	VG_(sprintf)(buf, "desc: Option: --separate-callers=%d\n",
		     CLG_(clo).separate_callers);
	my_fwrite(fd, (void*)buf, VG_(strlen)(buf));

	VG_(sprintf)(buf, "desc: Option: --dump-bbs=%s\n",
		     CLG_(clo).dump_bbs ? "yes" : "no");
	my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
	VG_(sprintf)(buf, "desc: Option: --separate-threads=%s\n",
		     CLG_(clo).separate_threads ? "yes" : "no");
	my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
#endif

	(*CLG_(cachesim).getdesc)(buf);
	my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
    }

    VG_(sprintf)(buf, "\ndesc: Timerange: Basic block %llu - %llu\n",
		 bbs_done, CLG_(stat).bb_executions);

    my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
    VG_(sprintf)(buf, "desc: Trigger: %s\n",
		 trigger ? trigger : (Char*)"Program termination");
    my_fwrite(fd, (void*)buf, VG_(strlen)(buf));

#if 0
   /* Output function specific config
    * FIXME */
   for (i = 0; i < N_FNCONFIG_ENTRIES; i++) {
       fnc = fnc_table[i];
       while (fnc) {
	   if (fnc->skip) {
	       VG_(sprintf)(buf, "desc: Option: --fn-skip=%s\n", fnc->name);
	       my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
	   }
	   if (fnc->dump_at_enter) {
	       VG_(sprintf)(buf, "desc: Option: --fn-dump-at-enter=%s\n",
			    fnc->name);
	       my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
	   }   
	   if (fnc->dump_at_leave) {
	       VG_(sprintf)(buf, "desc: Option: --fn-dump-at-leave=%s\n",
			    fnc->name);
	       my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
	   }
	   if (fnc->separate_callers != CLG_(clo).separate_callers) {
	       VG_(sprintf)(buf, "desc: Option: --separate-callers%d=%s\n",
			    fnc->separate_callers, fnc->name);
	       my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
	   }   
	   if (fnc->separate_recursions != CLG_(clo).separate_recursions) {
	       VG_(sprintf)(buf, "desc: Option: --separate-recs%d=%s\n",
			    fnc->separate_recursions, fnc->name);
	       my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
	   }   
	   fnc = fnc->next;
       }
   }
#endif

   /* "positions:" line */
   VG_(sprintf)(buf, "\npositions:%s%s%s\n",
		CLG_(clo).dump_instr ? " instr" : "",
		CLG_(clo).dump_bb    ? " bb" : "",
		CLG_(clo).dump_line  ? " line" : "");
   my_fwrite(fd, (void*)buf, VG_(strlen)(buf));

   /* "events:" line */
   i = VG_(sprintf)(buf, "events: ");
   CLG_(sprint_eventmapping)(buf+i, CLG_(dumpmap));
   my_fwrite(fd, (void*)buf, VG_(strlen)(buf));
   my_fwrite(fd, "\n", 1);

   /* summary lines */
   sum = CLG_(get_eventset_cost)( CLG_(sets).full );
   CLG_(zero_cost)(CLG_(sets).full, sum);
   if (CLG_(clo).separate_threads) {
     thread_info* ti = CLG_(get_current_thread)();
     CLG_(add_diff_cost)(CLG_(sets).full, sum, ti->lastdump_cost,
			   ti->states.entry[0]->cost);
   }
   else {
     /* This function is called once for thread 1, where
      * all costs are summed up when not dumping separate per thread.
      * But this is not true for summary: we need to add all threads.
      */
     int t;
     thread_info** thr = CLG_(get_threads)();
     for(t=1;t<VG_N_THREADS;t++) {
       if (!thr[t]) continue;
       CLG_(add_diff_cost)(CLG_(sets).full, sum,
			  thr[t]->lastdump_cost,
			  thr[t]->states.entry[0]->cost);
     }
   }
   fprint_cost_ln(fd, "summary: ", CLG_(dumpmap), sum);

   /* all dumped cost will be added to total_fcc */
   CLG_(init_cost_lz)( CLG_(sets).full, &dump_total_cost );

   my_fwrite(fd, "\n\n",2);

   if (VG_(clo_verbosity) > 1)
       VG_(message)(Vg_DebugMsg, "Dump to %s\n", filename);

   return fd;
}


static void close_dumpfile(int fd)
{
    if (fd <0) return;

    fprint_cost_ln(fd, "totals: ", CLG_(dumpmap),
		   dump_total_cost);
    //fprint_fcc_ln(fd, "summary: ", &dump_total_fcc);
    CLG_(add_cost_lz)(CLG_(sets).full, 
		     &CLG_(total_cost), dump_total_cost);

    fwrite_flush();    
    VG_(close)(fd);

    if (filename[0] == '.') {
	if (-1 == VG_(rename) (filename, filename+1)) {
	    /* Can not rename to correct file name: give out warning */
	    VG_(message)(Vg_DebugMsg, "Warning: Can not rename .%s to %s\n",
			 filename, filename);
       }
   }
}


/* Helper for print_bbccs */

static Int   print_fd;
static Char* print_trigger;
static Char  print_buf[BUF_LEN];

static void print_bbccs_of_thread(thread_info* ti)
{
  BBCC **p, **array;
  FnPos lastFnPos;
  AddrPos lastAPos;

  CLG_DEBUG(1, "+ print_bbccs(tid %d)\n", CLG_(current_tid));

  print_fd = new_dumpfile(print_buf, CLG_(current_tid), print_trigger);
  if (print_fd <0) {
    CLG_DEBUG(1, "- print_bbccs(tid %d): No output...\n", CLG_(current_tid));
    return;
  }

  p = array = prepare_dump();
  init_fpos(&lastFnPos);
  init_apos(&lastAPos, 0, 0, 0);

  if (p) while(1) {

    /* on context/function change, print old cost buffer before */
    if (lastFnPos.cxt && ((*p==0) ||				 
			 (lastFnPos.cxt != (*p)->cxt) ||
			 (lastFnPos.rec_index != (*p)->rec_index))) {
      if (!CLG_(is_zero_cost)( CLG_(sets).full, ccSum[currSum].cost )) {
	/* no need to switch buffers, as position is the same */
	fprint_apos(print_fd, &(ccSum[currSum].p), &lastAPos,
		    lastFnPos.cxt->fn[0]->file);
	fprint_fcost(print_fd, &ccSum[currSum], &lastAPos);
      }
      
      if (ccSum[currSum].p.file != lastFnPos.cxt->fn[0]->file) {
	/* switch back to file of function */
	VG_(sprintf)(print_buf, "fe=");
	print_file(print_buf+3, lastFnPos.cxt->fn[0]->file);
	my_fwrite(print_fd, (void*)print_buf, VG_(strlen)(print_buf));
      }
      my_fwrite(print_fd, "\n", 1);
    }
    
    if (*p == 0) break;
    
    if (print_fn_pos(print_fd, &lastFnPos, *p)) {
      
      /* new function */
      init_apos(&lastAPos, 0, 0, (*p)->cxt->fn[0]->file);
      init_fcost(&ccSum[0], 0, 0, 0);
      init_fcost(&ccSum[1], 0, 0, 0);
      currSum = 0;
    }
    
    if (CLG_(clo).dump_bbs) {
	/* FIXME: Specify Object of BB if different to object of fn */
	int i, pos = 0;
	ULong ecounter = (*p)->ecounter_sum;
	pos = VG_(sprintf)(print_buf, "bb=%#lx ", (*p)->bb->offset);
	for(i = 0; i<(*p)->bb->cjmp_count;i++) {
	    pos += VG_(sprintf)(print_buf+pos, "%d %llu ", 
				(*p)->bb->jmp[i].instr,
				ecounter);
	    ecounter -= (*p)->jmp[i].ecounter;
	}
	VG_(sprintf)(print_buf+pos, "%d %llu\n", 
		     (*p)->bb->instr_count,
		     ecounter);
	my_fwrite(print_fd, (void*)print_buf, VG_(strlen)(print_buf));
    }
    
    fprint_bbcc(print_fd, *p, &lastAPos);
    
    p++;
  }

  close_dumpfile(print_fd);
  if (array) VG_(free)(array);
  
  /* set counters of last dump */
  CLG_(copy_cost)( CLG_(sets).full, ti->lastdump_cost,
		  CLG_(current_state).cost );

  CLG_DEBUG(1, "- print_bbccs(tid %d)\n", CLG_(current_tid));
}


static void print_bbccs(Char* trigger, Bool only_current_thread)
{
  init_dump_array();
  init_debug_cache();

  print_fd = -1;
  print_trigger = trigger;

  if (!CLG_(clo).separate_threads) {
    /* All BBCC/JCC costs is stored for thread 1 */
    Int orig_tid = CLG_(current_tid);

    CLG_(switch_thread)(1);
    print_bbccs_of_thread( CLG_(get_current_thread)() );
    CLG_(switch_thread)(orig_tid);
  }
  else if (only_current_thread)
    print_bbccs_of_thread( CLG_(get_current_thread)() );
  else
    CLG_(forall_threads)(print_bbccs_of_thread);

  free_dump_array();
}


void CLG_(dump_profile)(Char* trigger, Bool only_current_thread)
{
   CLG_DEBUG(2, "+ dump_profile(Trigger '%s')\n",
	    trigger ? trigger : (Char*)"Prg.Term.");

   CLG_(init_dumps)();

   if (VG_(clo_verbosity) > 1)
       VG_(message)(Vg_DebugMsg, "Start dumping at BB %llu (%s)...\n",
		    CLG_(stat).bb_executions,
		    trigger ? trigger : (Char*)"Prg.Term.");

   out_counter++;

   print_bbccs(trigger, only_current_thread);

   bbs_done = CLG_(stat).bb_executions++;

   if (VG_(clo_verbosity) > 1)
     VG_(message)(Vg_DebugMsg, "Dumping done.\n");
}

/* Copy command to cmd buffer. We want to original command line
 * (can change at runtime)
 */
static
void init_cmdbuf(void)
{
  Int i,j,size = 0;
  HChar* argv;

  if (VG_(args_the_exename)) {
      CLG_ASSERT( VG_(strlen)( VG_(args_the_exename) ) < BUF_LEN-1);
      size = VG_(sprintf)(cmdbuf, " %s", VG_(args_the_exename));
  }

  for(i = 0; i < VG_(sizeXA)( VG_(args_for_client) ); i++) {
      argv = * (HChar**) VG_(indexXA)( VG_(args_for_client), i );
      if (!argv) continue;
      if ((size>0) && (size < BUF_LEN)) cmdbuf[size++] = ' ';
      for(j=0;argv[j]!=0;j++)
	  if (size < BUF_LEN) cmdbuf[size++] = argv[j];
  }

  if (size >= BUF_LEN) size = BUF_LEN-1;
  cmdbuf[size] = 0;
}

/*
 * Set up file names for dump output: <out_directory>, <out_file>.
 * <out_file> is derived from the output format string, which defaults
 * to "callgrind.out.%p", where %p is replaced with the PID.
 * For the final file name, on intermediate dumps a counter is appended,
 * and further, if separate dumps per thread are requested, the thread ID.
 *
 * <out_file> always starts with a full absolute path.
 * If the output format string represents a relative path, the current
 * working directory at program start is used.
 *
 * This function has to be called every time a profile dump is generated
 * to be able to react on PID changes.
 */
void CLG_(init_dumps)()
{
   Int lastSlash, i;
   SysRes res;

   static int thisPID = 0;
   int currentPID = VG_(getpid)();
   if (currentPID == thisPID) {
       /* already initialized, and no PID change */
       CLG_ASSERT(out_file != 0);
       return;
   }
   thisPID = currentPID;
   
   if (!CLG_(clo).out_format)
     CLG_(clo).out_format = DEFAULT_OUTFORMAT;

   /* If a file name was already set, clean up before */
   if (out_file) {
       VG_(free)(out_file);
       VG_(free)(out_directory);
       VG_(free)(filename);
       out_counter = 0;
   }

   // Setup output filename.
   out_file =
       VG_(expand_file_name)("--callgrind-out-file", CLG_(clo).out_format);

   /* get base directory for dump/command/result files */
   CLG_ASSERT(out_file[0] == '/');
   lastSlash = 0;
   i = 1;
   while(out_file[i]) {
       if (out_file[i] == '/') lastSlash = i;
       i++;
   }
   i = lastSlash;
   out_directory = (Char*) CLG_MALLOC("cl.dump.init_dumps.1", i+1);
   VG_(strncpy)(out_directory, out_file, i);
   out_directory[i] = 0;

   /* allocate space big enough for final filenames */
   filename = (Char*) CLG_MALLOC("cl.dump.init_dumps.2",
                                 VG_(strlen)(out_file)+32);
   CLG_ASSERT(filename != 0);
       
   /* Make sure the output base file can be written.
    * This is used for the dump at program termination.
    * We stop with an error here if we can not create the
    * file: This is probably because of missing rights,
    * and trace parts wouldn't be allowed to be written, too.
    */ 
    VG_(strcpy)(filename, out_file);
    res = VG_(open)(filename, VKI_O_WRONLY|VKI_O_TRUNC, 0);
    if (sr_isError(res)) { 
	res = VG_(open)(filename, VKI_O_CREAT|VKI_O_WRONLY,
		       VKI_S_IRUSR|VKI_S_IWUSR);
	if (sr_isError(res)) {
	    file_err(); 
	}
    }
    if (!sr_isError(res)) VG_(close)( (Int)sr_Res(res) );

    if (!dumps_initialized)
	init_cmdbuf();

    dumps_initialized = True;
}
