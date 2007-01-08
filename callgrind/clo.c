/*
   This file is part of Callgrind, a Valgrind skin for call graph
   profiling programs.

   Copyright (C) 2002-2007, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

   This skin is derived from and contains lot of code from Cachegrind
   Copyright (C) 2002 Nicholas Nethercote (njn25@cam.ac.uk)

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

#include "config.h" // for VG_PREFIX

#include "global.h"



/*------------------------------------------------------------*/
/*--- Function specific configuration options              ---*/
/*------------------------------------------------------------*/

/* Special value for separate_callers: automatic = adaptive */
#define CONFIG_AUTO    -1

#define CONFIG_DEFAULT -1
#define CONFIG_FALSE    0
#define CONFIG_TRUE     1

/* Logging configuration for a function */
struct _fn_config {
    Int dump_before;
    Int dump_after;
    Int zero_before;
    Int toggle_collect;

    Int skip;    /* Handle CALL to this function as JMP (= Skip)? */
    Int group;   /* don't change caller dependency inside group !=0 */
    Int pop_on_jump; 

    Int separate_callers;    /* separate logging dependent on caller  */
    Int separate_recursions; /* separate logging of rec. levels       */

#if CLG_ENABLE_DEBUG
    Int verbosity; /* Change debug verbosity level while in function */
#endif
};

/* Configurations for function name prefix patterns.
 * Currently, only very limit patterns are possible:
 * Exact prefix patterns and "*::" are allowed.
 * E.g.
 *  - "abc" matches all functions starting with "abc".
 *  - "abc*::def" matches all functions starting with "abc" and
 *    starting with "def" after the first "::" separator.
 *  - "*::print(" matches C++ methods "print" in all classes
 *    without namespace. I.e. "*" doesn't match a "::".
 *
 * We build a trie from patterns, and for a given function, we
 * go down the tree and apply all non-default configurations.
 */


#define NODE_DEGREE 30

/* node of compressed trie search structure */
typedef struct _config_node config_node;
struct _config_node {
  Int length;
    
  fn_config* config;
  config_node* sub_node[NODE_DEGREE];
  config_node* next;
  config_node* wild_star;
  config_node* wild_char;

  Char name[1];
};

/* root of trie */
static config_node* fn_configs = 0;

static __inline__ 
fn_config* new_fnc(void)
{
   fn_config* new = (fn_config*) CLG_MALLOC(sizeof(fn_config));

   new->dump_before  = CONFIG_DEFAULT;
   new->dump_after   = CONFIG_DEFAULT;
   new->zero_before  = CONFIG_DEFAULT;
   new->toggle_collect = CONFIG_DEFAULT;
   new->skip         = CONFIG_DEFAULT;
   new->pop_on_jump  = CONFIG_DEFAULT;
   new->group        = CONFIG_DEFAULT;
   new->separate_callers    = CONFIG_DEFAULT;
   new->separate_recursions = CONFIG_DEFAULT;

#if CLG_ENABLE_DEBUG
   new->verbosity    = CONFIG_DEFAULT;
#endif

   return new;
}


static config_node* new_config(Char* name, int length)
{
    int i;
    config_node* node = (config_node*) CLG_MALLOC(sizeof(config_node) + length);

    for(i=0;i<length;i++) {
      if (name[i] == 0) break;
      node->name[i] = name[i];
    }
    node->name[i] = 0;

    node->length = length;
    node->config = 0;
    for(i=0;i<NODE_DEGREE;i++)
	node->sub_node[i] = 0;
    node->next = 0;
    node->wild_char = 0;
    node->wild_star = 0;

    CLG_DEBUG(3, "   new_config('%s', len %d)\n", node->name, length);

    return node;
}

static __inline__
Bool is_wild(Char n)
{
  return (n == '*') || (n == '?');
}

/* Recursively build up function matching tree (prefix tree).
 * Returns function config object for pattern <name>
 * and starting at tree node <*pnode>.
 *
 * Tree nodes (config_node) are created as needed,
 * tree root is stored into <*pnode>, and the created
 * leaf (fn_config) for the given pattern is returned.
 */
static fn_config* get_fnc2(config_node* node, Char* name)
{
  config_node *new_sub, *n, *nprev;
  int offset, len;

  CLG_DEBUG(3, "  get_fnc2(%p, '%s')\n", node, name);

  if (name[0] == 0) {
    if (!node->config) node->config = new_fnc();
    return node->config;
  }

  if (is_wild(*name)) {
    if (*name == '*') {
      while(name[1] == '*') name++;
      new_sub = node->wild_star;
    }
    else
      new_sub = node->wild_char;

    if (!new_sub) {
      new_sub = new_config(name, 1);
      if (*name == '*')
	node->wild_star = new_sub;
      else
	node->wild_char = new_sub;
    }

    return get_fnc2( new_sub, name+1);
  }

  n = node->sub_node[ name[0]%NODE_DEGREE ];
  nprev = 0;
  len = 0;
  while(n) {
    for(len=0; name[len] == n->name[len]; len++);
    if (len>0) break;
    nprev = n;
    n = n->next;
  }

  if (!n) {
    len = 1;
    while(name[len] && (!is_wild(name[len]))) len++;
    new_sub = new_config(name, len);
    new_sub->next = node->sub_node[ name[0]%NODE_DEGREE ];
    node->sub_node[ name[0]%NODE_DEGREE ] = new_sub;	

    if (name[len] == 0) {
      new_sub->config = new_fnc();
      return new_sub->config;
    }
    
    /* recurse on wildcard */
    return get_fnc2( new_sub, name+len);
  }

  if (len < n->length) {

    /* split up the subnode <n> */
    config_node *new_node;
    int i;

    new_node = new_config(n->name, len);
    if (nprev)
      nprev->next = new_node;
    else
      node->sub_node[ n->name[0]%NODE_DEGREE ] = new_node;
    new_node->next = n->next;

    new_node->sub_node[ n->name[len]%NODE_DEGREE ] = n;

    for(i=0, offset=len; offset < n->length; i++, offset++)
      n->name[i] = n->name[offset];
    n->name[i] = 0;
    n->length = i;

    name += len;
    offset = 0;
    while(name[offset] && (!is_wild(name[offset]))) offset++;
    new_sub  = new_config(name, offset);
    /* this sub_node of new_node could already be set: chain! */
    new_sub->next = new_node->sub_node[ name[0]%NODE_DEGREE ];
    new_node->sub_node[ name[0]%NODE_DEGREE ] = new_sub;

    if (name[offset]==0) {
      new_sub->config = new_fnc();
      return new_sub->config;
    }

    /* recurse on wildcard */
    return get_fnc2( new_sub, name+offset);
  }

  name += n->length;

  if (name[0] == 0) {
    /* name and node name are the same */
    if (!n->config) n->config = new_fnc();
    return n->config;
  }

  offset = 1;
  while(name[offset] && (!is_wild(name[offset]))) offset++;

  new_sub = new_config(name, offset);
  new_sub->next = n->sub_node[ name[offset]%NODE_DEGREE ];
  n->sub_node[ name[offset]%NODE_DEGREE ] = new_sub;	

  return get_fnc2(new_sub, name+offset);
}

static void print_config_node(int s, config_node* node)
{
  config_node* n;
  int i;

  if (node != fn_configs) {
    char sp[] = "                                        ";

    if (s>40) s=40;
    VG_(printf)(sp+40-s);
    VG_(printf)("'%s'/%d\n", node->name, node->length);
  }
  for(i=0;i<NODE_DEGREE;i++) {
    n = node->sub_node[i];
    while(n) {
      print_config_node(s+1, n);
      n = n->next;
    }
  }
  if (node->wild_char) print_config_node(s+1, node->wild_char);
  if (node->wild_star) print_config_node(s+1, node->wild_star);
}

/* get a function config for a name pattern (from command line) */
static fn_config* get_fnc(Char* name)
{
  fn_config* fnc;

  CLG_DEBUG(3, " +get_fnc(%s)\n", name);
  if (fn_configs == 0)
    fn_configs = new_config(name, 0);
  fnc =  get_fnc2(fn_configs, name);

  CLG_DEBUGIF(3) {
    CLG_DEBUG(3, " -get_fnc(%s):\n", name);
    print_config_node(3, fn_configs);
  }
  return fnc;
}

  

static void update_fn_config1(fn_node* fn, fn_config* fnc)
{
    if (fnc->dump_before != CONFIG_DEFAULT)
	fn->dump_before = (fnc->dump_before == CONFIG_TRUE);

    if (fnc->dump_after != CONFIG_DEFAULT)
	fn->dump_after = (fnc->dump_after == CONFIG_TRUE);

    if (fnc->zero_before != CONFIG_DEFAULT)
	fn->zero_before = (fnc->zero_before == CONFIG_TRUE);

    if (fnc->toggle_collect != CONFIG_DEFAULT)
	fn->toggle_collect = (fnc->toggle_collect == CONFIG_TRUE);

    if (fnc->skip != CONFIG_DEFAULT)
	fn->skip = (fnc->skip == CONFIG_TRUE);

    if (fnc->pop_on_jump != CONFIG_DEFAULT)
	fn->pop_on_jump = (fnc->pop_on_jump == CONFIG_TRUE);

    if (fnc->group != CONFIG_DEFAULT)
	fn->group = fnc->group;

    if (fnc->separate_callers != CONFIG_DEFAULT)
	fn->separate_callers = fnc->separate_callers;

    if (fnc->separate_recursions != CONFIG_DEFAULT)
	fn->separate_recursions = fnc->separate_recursions;

#if CLG_ENABLE_DEBUG
    if (fnc->verbosity != CONFIG_DEFAULT)
	fn->verbosity = fnc->verbosity;
#endif
}

/* Recursively go down the function matching tree,
 * looking for a match to <name>. For every matching leaf,
 * <fn> is updated with the pattern config.
 */
static void update_fn_config2(fn_node* fn, Char* name, config_node* node)
{
    config_node* n;

    CLG_DEBUG(3, "  update_fn_config2('%s', node '%s'): \n",
	     name, node->name);
    if ((*name == 0) && node->config) {
      CLG_DEBUG(3, "Found!\n");
      update_fn_config1(fn, node->config);
      return;
    }

    n = node->sub_node[ name[0]%NODE_DEGREE ];
    while(n) {
      if (VG_(strncmp)(name, n->name, n->length)==0) break;
      n = n->next;
    }
    if (n) update_fn_config2(fn, name+n->length, n);
    
    if (node->wild_char)
      update_fn_config2(fn, name+1, node->wild_char);

    if (node->wild_star) {
      while(*name) {
	update_fn_config2(fn, name, node->wild_star);
	name++;
      }
      update_fn_config2(fn, name, node->wild_star);
    }
}

/* Update function config according to configs of name prefixes */
void CLG_(update_fn_config)(fn_node* fn)
{
    CLG_DEBUG(3, "  update_fn_config('%s')\n", fn->name);
    if (fn_configs)
      update_fn_config2(fn, fn->name, fn_configs);
}


/*--------------------------------------------------------------------*/
/*--- Command line processing                                      ---*/
/*--------------------------------------------------------------------*/

static Char* getUInt(Char* s, UInt* pn)
{
    UInt n = 0;
    while((*s >='0') && (*s <='9')) {
	n = 10*n + (*s-'0');
	s++;
    }
    if (pn) *pn = n;
    return s;
}

__attribute__((unused))
static UWord getUWord(Char* s)
{
    UWord n = 0;
    Bool isHex = False;

    if ((s[0] == '0') && (s[1] == 'x')) {
	isHex = True;
	s += 2;
    }

    if (!isHex) {
	while((*s >='0') && (*s <='9')) {
	    n = 10*n + (*s-'0');
	    s++;
	}
    }
    else {
	while(1) {
	    if ((*s >='0') && (*s <='9')) {
		n = 16*n + (*s-'0');
		s++;
		continue;
	    }
	    if ((*s >='a') && (*s <='f')) {
		n = 16*n + (*s-'a'+10);
		s++;
		continue;
	    }
	    if ((*s >='A') && (*s <='F')) {
		n = 16*n + (*s-'A'+10);
		s++;
		continue;
	    }
	    break;
	}
    }

    return n;
}

Bool CLG_(process_cmd_line_option)(Char* arg)
{
   if (0 == VG_(strcmp)(arg, "--skip-plt=yes"))
       CLG_(clo).skip_plt = True;
   else if (0 == VG_(strcmp)(arg, "--skip-plt=no"))
       CLG_(clo).skip_plt = False;

   else if (0 == VG_(strcmp)(arg, "--collect-jumps=yes"))
       CLG_(clo).collect_jumps = True;
   else if (0 == VG_(strcmp)(arg, "--collect-jumps=no"))
       CLG_(clo).collect_jumps = False;
   /* compatibility alias, deprecated option */
   else if (0 == VG_(strcmp)(arg, "--trace-jump=yes"))
       CLG_(clo).collect_jumps = True;
   else if (0 == VG_(strcmp)(arg, "--trace-jump=no"))
       CLG_(clo).collect_jumps = False;

   else if (0 == VG_(strcmp)(arg, "--combine-dumps=yes"))
       CLG_(clo).combine_dumps = True;
   else if (0 == VG_(strcmp)(arg, "--combine-dumps=no"))
       CLG_(clo).combine_dumps = False;

   else if (0 == VG_(strcmp)(arg, "--collect-atstart=yes"))
       CLG_(clo).collect_atstart = True;
   else if (0 == VG_(strcmp)(arg, "--collect-atstart=no"))
       CLG_(clo).collect_atstart = False;

   else if (0 == VG_(strcmp)(arg, "--instr-atstart=yes"))
       CLG_(clo).instrument_atstart = True;
   else if (0 == VG_(strcmp)(arg, "--instr-atstart=no"))
       CLG_(clo).instrument_atstart = False;

   else if (0 == VG_(strcmp)(arg, "--separate-threads=yes"))
       CLG_(clo).separate_threads = True;
   else if (0 == VG_(strcmp)(arg, "--separate-threads=no"))
       CLG_(clo).separate_threads = False;

   else if (0 == VG_(strcmp)(arg, "--compress-strings=yes"))
       CLG_(clo).compress_strings = True;
   else if (0 == VG_(strcmp)(arg, "--compress-strings=no"))
       CLG_(clo).compress_strings = False;

   else if (0 == VG_(strcmp)(arg, "--compress-mangled=yes"))
       CLG_(clo).compress_mangled = True;
   else if (0 == VG_(strcmp)(arg, "--compress-mangled=no"))
       CLG_(clo).compress_mangled = False;

   else if (0 == VG_(strcmp)(arg, "--compress-pos=yes"))
       CLG_(clo).compress_pos = True;
   else if (0 == VG_(strcmp)(arg, "--compress-pos=no"))
       CLG_(clo).compress_pos = False;

   else if (0 == VG_(strncmp)(arg, "--fn-skip=", 10)) {
       fn_config* fnc = get_fnc(arg+10);
       fnc->skip = CONFIG_TRUE;
   }

   else if (0 == VG_(strncmp)(arg, "--dump-before=", 14)) {
       fn_config* fnc = get_fnc(arg+14);
       fnc->dump_before = CONFIG_TRUE;
   }

   else if (0 == VG_(strncmp)(arg, "--zero-before=", 14)) {
       fn_config* fnc = get_fnc(arg+14);
       fnc->zero_before = CONFIG_TRUE;
   }

   else if (0 == VG_(strncmp)(arg, "--dump-after=", 13)) {
       fn_config* fnc = get_fnc(arg+13);
       fnc->dump_after = CONFIG_TRUE;
   }

   else if (0 == VG_(strncmp)(arg, "--toggle-collect=", 17)) {
       fn_config* fnc = get_fnc(arg+17);
       fnc->toggle_collect = CONFIG_TRUE;
       /* defaults to initial collection off */
       CLG_(clo).collect_atstart = False;
   }

   else if (0 == VG_(strncmp)(arg, "--separate-recs=", 16))
        CLG_(clo).separate_recursions = (Int)VG_(atoll)(&arg[16]);

   /* change handling of a jump between functions to ret+call */
   else if (0 == VG_(strcmp)(arg, "--pop-on-jump")) {
        CLG_(clo).pop_on_jump = True;
   }
   else if (0 == VG_(strncmp)(arg, "--pop-on-jump=", 14)) {
       fn_config* fnc = get_fnc(arg+14);
       fnc->pop_on_jump = CONFIG_TRUE;
   }

#if CLG_ENABLE_DEBUG
   else if (0 == VG_(strncmp)(arg, "--ct-verbose=", 13))
        CLG_(clo).verbose = (Int)VG_(atoll)(&arg[13]);

   else if (0 == VG_(strncmp)(arg, "--ct-vstart=", 12))
        CLG_(clo).verbose_start = (ULong)VG_(atoll)(&arg[12]);

   else if (0 == VG_(strncmp)(arg, "--ct-verbose", 12)) {
       UInt n;
       fn_config* fnc;
       Char* s = getUInt(arg+12, &n);
       if ((n == 0) || *s != '=') return False;
       fnc = get_fnc(s+1);
       fnc->verbosity = n;
   }
#endif

   else if (0 == VG_(strncmp)(arg, "--separate-callers=", 19)) {
     if (0 == VG_(strcmp)(arg+19, "auto"))
       CLG_(clo).separate_callers = CONFIG_AUTO;
     else
       CLG_(clo).separate_callers = (Int)VG_(atoll)(&arg[19]);
   }

   else if (0 == VG_(strncmp)(arg, "--fn-group", 10)) {
       UInt n;
       fn_config* fnc;
       Char* s = getUInt(arg+10, &n);
       if ((n == 0) || *s != '=') return False;
       fnc = get_fnc(s+1);
       fnc->group = n;
   }

   else if (0 == VG_(strncmp)(arg, "--separate-callers", 18)) {
       UInt n;
       fn_config* fnc;
       Char* s = getUInt(arg+18, &n);
       if ((n == 0) || *s != '=') return False;
       fnc = get_fnc(s+1);
       fnc->separate_callers = n;
   }

   else if (0 == VG_(strncmp)(arg, "--separate-recs", 15)) {
       UInt n;
       fn_config* fnc;
       Char* s = getUInt(arg+15, &n);
       if ((n == 0) || *s != '=') return False;
       fnc = get_fnc(s+1);
       fnc->separate_recursions = n;
   }

   else if (0 == VG_(strncmp)(arg, "--base=", 7))
       CLG_(clo).filename_base = VG_(strdup)(arg+7);

   else if (0 == VG_(strcmp)(arg, "--mangle-names=yes"))
       CLG_(clo).mangle_names = True;
   else if (0 == VG_(strcmp)(arg, "--mangle-names=no"))
       CLG_(clo).mangle_names = False;

   else if (0 == VG_(strcmp)(arg, "--skip-direct-rec=yes"))
       CLG_(clo).skip_direct_recursion = True;
   else if (0 == VG_(strcmp)(arg, "--skip-direct-rec=no"))
       CLG_(clo).skip_direct_recursion = False;

   else if (0 == VG_(strcmp)(arg, "--dump-bbs=yes"))
       CLG_(clo).dump_bbs = True;
   else if (0 == VG_(strcmp)(arg, "--dump-bbs=no"))
       CLG_(clo).dump_bbs = False;

   else if (0 == VG_(strcmp)(arg, "--dump-line=yes"))
       CLG_(clo).dump_line = True;
   else if (0 == VG_(strcmp)(arg, "--dump-line=no"))
       CLG_(clo).dump_line = False;

   else if (0 == VG_(strcmp)(arg, "--dump-instr=yes"))
       CLG_(clo).dump_instr = True;
   else if (0 == VG_(strcmp)(arg, "--dump-instr=no"))
       CLG_(clo).dump_instr = False;

   else if (0 == VG_(strcmp)(arg, "--dump-bb=yes"))
       CLG_(clo).dump_bb = True;
   else if (0 == VG_(strcmp)(arg, "--dump-bb=no"))
       CLG_(clo).dump_bb = False;

   else if (0 == VG_(strncmp)(arg, "--dump-every-bb=", 16))
        CLG_(clo).dump_every_bb = (ULong)VG_(atoll)(&arg[16]);


   else if (0 == VG_(strcmp)(arg, "--collect-alloc=yes"))
       CLG_(clo).collect_alloc = True;
   else if (0 == VG_(strcmp)(arg, "--collect-alloc=no"))
       CLG_(clo).collect_alloc = False;

   else if (0 == VG_(strcmp)(arg, "--collect-systime=yes"))
       CLG_(clo).collect_systime = True;
   else if (0 == VG_(strcmp)(arg, "--collect-systime=no"))
       CLG_(clo).collect_systime = False;

   else if (0 == VG_(strcmp)(arg, "--simulate-cache=yes"))
       CLG_(clo).simulate_cache = True;
   else if (0 == VG_(strcmp)(arg, "--simulate-cache=no"))
       CLG_(clo).simulate_cache = False;

   else {
       Bool isCachesimOption = (*CLG_(cachesim).parse_opt)(arg);

       /* cache simulator is used if a simulator option is given */
       if (isCachesimOption)
	   CLG_(clo).simulate_cache = True;

       return isCachesimOption;
   }

   return True;
}

void CLG_(print_usage)(void)
{
   VG_(printf)(
"\n   dump creation options:\n"
"    --base=<prefix>           Prefix for profile files [" DEFAULT_DUMPNAME "]\n"
"    --dump-line=no|yes        Dump source lines of costs? [yes]\n"
"    --dump-instr=no|yes       Dump instruction address of costs? [no]\n"
"    --compress-strings=no|yes Compress strings in profile dump? [yes]\n"
"    --compress-pos=no|yes     Compress positions in profile dump? [yes]\n"
"    --combine-dumps=no|yes    Concat all dumps into same file [no]\n"
#if CLG_EXPERIMENTAL
"    --compress-events=no|yes  Compress events in profile dump? [no]\n"
"    --dump-bb=no|yes          Dump basic block address of costs? [no]\n"
"    --dump-bbs=no|yes         Dump basic block info? [no]\n"
"    --dump-skipped=no|yes     Dump info on skipped functions in calls? [no]\n"
"    --mangle-names=no|yes     Mangle separation into names? [yes]\n"
#endif

"\n   activity options (for interactivity use callgrind_control):\n"
"    --dump-every-bb=<count>   Dump every <count> basic blocks [0=never]\n"
"    --dump-before=<func>      Dump when entering function\n"
"    --zero-before=<func>      Zero all costs when entering function\n"
"    --dump-after=<func>       Dump when leaving function\n"
#if CLG_EXPERIMENTAL
"    --dump-objs=no|yes        Dump static object information [no]\n"
#endif

"\n   data collection options:\n"
"    --instr-atstart=no|yes    Do instrumentation at callgrind start [yes]\n"
"    --collect-atstart=no|yes  Collect at process/thread start [yes]\n"
"    --toggle-collect=<func>   Toggle collection on enter/leave function\n"
"    --collect-jumps=no|yes    Collect jumps? [no]\n"
#if CLG_EXPERIMENTAL
"    --collect-alloc=no|yes    Collect memory allocation info? [no]\n"
#endif
"    --collect-systime=no|yes  Collect system call time info? [no]\n"

"\n   cost entity separation options:\n"
"    --separate-threads=no|yes Separate data per thread [no]\n"
"    --separate-callers=<n>    Separate functions by call chain length [0]\n"
"    --separate-recs=<n>       Separate function recursions upto level [2]\n"
"    --skip-plt=no|yes         Ignore calls to/from PLT sections? [yes]\n"
"    --separate-recs<n>=<f>    Separate <n> recursions for function <f>\n"
"    --separate-callers<n>=<f> Separate <n> callers for function <f>\n"
"    --skip-direct-rec=no|yes  Ignore direct recursions? [yes]\n"
"    --fn-skip=<function>      Ignore calls to/from function?\n"
#if CLG_EXPERIMENTAL
"    --fn-group<no>=<func>     Put function into separation group <no>\n"
#endif
    );

   (*CLG_(cachesim).print_opts)();

//   VG_(printf)("\n"
//	       "  For full callgrind documentation, see\n"
//	       "  "VG_PREFIX"/share/doc/callgrind/html/callgrind.html\n\n");
}

void CLG_(print_debug_usage)(void)
{
    VG_(printf)(

#if CLG_ENABLE_DEBUG
"    --ct-verbose=<level>       Verbosity of standard debug output [0]\n"
"    --ct-vstart=<BB number>    Only be verbose after basic block [0]\n"
"    --ct-verbose<level>=<func> Verbosity while in <func>\n"
#else
"    (none)\n"
#endif

    );
}


void CLG_(set_clo_defaults)(void)
{
  /* Default values for command line arguments */

  /* dump options */
  CLG_(clo).filename_base    = 0;
  CLG_(clo).combine_dumps    = False;
  CLG_(clo).compress_strings = True;
  CLG_(clo).compress_mangled = False;
  CLG_(clo).compress_events  = False;
  CLG_(clo).compress_pos     = True;
  CLG_(clo).mangle_names     = True;
  CLG_(clo).dump_line        = True;
  CLG_(clo).dump_instr       = False;
  CLG_(clo).dump_bb          = False;
  CLG_(clo).dump_bbs         = False;

  CLG_(clo).dump_every_bb    = 0;

  /* Collection */
  CLG_(clo).separate_threads = False;
  CLG_(clo).collect_atstart  = True;
  CLG_(clo).collect_jumps    = False;
  CLG_(clo).collect_alloc    = False;
  CLG_(clo).collect_systime  = False;

  CLG_(clo).skip_plt         = True;
  CLG_(clo).separate_callers = 0;
  CLG_(clo).separate_recursions = 2;
  CLG_(clo).skip_direct_recursion = False;

  /* Instrumentation */
  CLG_(clo).instrument_atstart = True;
  CLG_(clo).simulate_cache = False;

  /* Call graph */
  CLG_(clo).pop_on_jump = False;

#if CLG_ENABLE_DEBUG
  CLG_(clo).verbose = 0;
  CLG_(clo).verbose_start = 0;
#endif
}
