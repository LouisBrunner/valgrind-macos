/*
   This file is part of Callgrind, a Valgrind tool for call graph
   profiling programs.

   Copyright (C) 2002-2012, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

   This tool is derived from and contains lot of code from Cachegrind
   Copyright (C) 2002-2012 Nicholas Nethercote (njn@valgrind.org)

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
   fn_config* fnc = (fn_config*) CLG_MALLOC("cl.clo.nf.1",
                                            sizeof(fn_config));

   fnc->dump_before  = CONFIG_DEFAULT;
   fnc->dump_after   = CONFIG_DEFAULT;
   fnc->zero_before  = CONFIG_DEFAULT;
   fnc->toggle_collect = CONFIG_DEFAULT;
   fnc->skip         = CONFIG_DEFAULT;
   fnc->pop_on_jump  = CONFIG_DEFAULT;
   fnc->group        = CONFIG_DEFAULT;
   fnc->separate_callers    = CONFIG_DEFAULT;
   fnc->separate_recursions = CONFIG_DEFAULT;

#if CLG_ENABLE_DEBUG
   fnc->verbosity    = CONFIG_DEFAULT;
#endif

   return fnc;
}


static config_node* new_config(Char* name, int length)
{
    int i;
    config_node* node = (config_node*) CLG_MALLOC("cl.clo.nc.1",
                                                  sizeof(config_node) + length);

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
  new_sub->next = n->sub_node[ name[0]%NODE_DEGREE ];
  n->sub_node[ name[0]%NODE_DEGREE ] = new_sub;

  return get_fnc2(new_sub, name+offset);
}

static void print_config_node(int depth, int hash, config_node* node)
{
  config_node* n;
  int i;

  if (node != fn_configs) {
    char sp[] = "                                        ";

    if (depth>40) depth=40;
    VG_(printf)("%s", sp+40-depth);
    if (hash >=0) VG_(printf)(" [hash %2d]", hash);
    else if (hash == -2) VG_(printf)(" [wildc ?]");
    else if (hash == -3) VG_(printf)(" [wildc *]");
    VG_(printf)(" '%s' (len %d)\n", node->name, node->length);
  }
  for(i=0;i<NODE_DEGREE;i++) {
    n = node->sub_node[i];
    while(n) {
      print_config_node(depth+1, i, n);
      n = n->next;
    }
  }
  if (node->wild_char) print_config_node(depth+1, -2, node->wild_char);
  if (node->wild_star) print_config_node(depth+1, -3, node->wild_star);
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
    print_config_node(3, -1, fn_configs);
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
      CLG_DEBUG(3, "   found!\n");
      update_fn_config1(fn, node->config);
      return;
    }

    n = node->sub_node[ name[0]%NODE_DEGREE ];
    while(n) {
      if (VG_(strncmp)(name, n->name, n->length)==0) break;
      n = n->next;
    }
    if (n) {
	CLG_DEBUG(3, "   '%s' matching at hash %d\n",
		  n->name, name[0]%NODE_DEGREE);
	update_fn_config2(fn, name+n->length, n);
    }
    
    if (node->wild_char) {
	CLG_DEBUG(3, "   skip '%c' for wildcard '?'\n", *name);
	update_fn_config2(fn, name+1, node->wild_char);
    }

    if (node->wild_star) {
      CLG_DEBUG(3, "   wildcard '*'\n");
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

Bool CLG_(process_cmd_line_option)(Char* arg)
{
   Char* tmp_str;

   if      VG_BOOL_CLO(arg, "--skip-plt", CLG_(clo).skip_plt) {}

   else if VG_BOOL_CLO(arg, "--collect-jumps", CLG_(clo).collect_jumps) {}
   /* compatibility alias, deprecated option */
   else if VG_BOOL_CLO(arg, "--trace-jump",    CLG_(clo).collect_jumps) {}

   else if VG_BOOL_CLO(arg, "--combine-dumps", CLG_(clo).combine_dumps) {}

   else if VG_BOOL_CLO(arg, "--collect-atstart", CLG_(clo).collect_atstart) {}

   else if VG_BOOL_CLO(arg, "--instr-atstart", CLG_(clo).instrument_atstart) {}

   else if VG_BOOL_CLO(arg, "--separate-threads", CLG_(clo).separate_threads) {}

   else if VG_BOOL_CLO(arg, "--compress-strings", CLG_(clo).compress_strings) {}
   else if VG_BOOL_CLO(arg, "--compress-mangled", CLG_(clo).compress_mangled) {}
   else if VG_BOOL_CLO(arg, "--compress-pos",     CLG_(clo).compress_pos) {}

   else if VG_STR_CLO(arg, "--fn-skip", tmp_str) {
       fn_config* fnc = get_fnc(tmp_str);
       fnc->skip = CONFIG_TRUE;
   }

   else if VG_STR_CLO(arg, "--dump-before", tmp_str) {
       fn_config* fnc = get_fnc(tmp_str);
       fnc->dump_before = CONFIG_TRUE;
   }

   else if VG_STR_CLO(arg, "--zero-before", tmp_str) {
       fn_config* fnc = get_fnc(tmp_str);
       fnc->zero_before = CONFIG_TRUE;
   }

   else if VG_STR_CLO(arg, "--dump-after", tmp_str) {
       fn_config* fnc = get_fnc(tmp_str);
       fnc->dump_after = CONFIG_TRUE;
   }

   else if VG_STR_CLO(arg, "--toggle-collect", tmp_str) {
       fn_config* fnc = get_fnc(tmp_str);
       fnc->toggle_collect = CONFIG_TRUE;
       /* defaults to initial collection off */
       CLG_(clo).collect_atstart = False;
   }

   else if VG_INT_CLO(arg, "--separate-recs", CLG_(clo).separate_recursions) {}

   /* change handling of a jump between functions to ret+call */
   else if VG_XACT_CLO(arg, "--pop-on-jump", CLG_(clo).pop_on_jump, True) {}
   else if VG_STR_CLO( arg, "--pop-on-jump", tmp_str) {
       fn_config* fnc = get_fnc(tmp_str);
       fnc->pop_on_jump = CONFIG_TRUE;
   }

#if CLG_ENABLE_DEBUG
   else if VG_INT_CLO(arg, "--ct-verbose", CLG_(clo).verbose) {}
   else if VG_INT_CLO(arg, "--ct-vstart",  CLG_(clo).verbose_start) {}

   else if VG_STREQN(12, arg, "--ct-verbose") {
       fn_config* fnc;
       Char* s;
       UInt n = VG_(strtoll10)(arg+12, &s);
       if ((n <= 0) || *s != '=') return False;
       fnc = get_fnc(s+1);
       fnc->verbosity = n;
   }
#endif

   else if VG_XACT_CLO(arg, "--separate-callers=auto", 
                            CLG_(clo).separate_callers, CONFIG_AUTO) {}
   else if VG_INT_CLO( arg, "--separate-callers", 
                            CLG_(clo).separate_callers) {}

   else if VG_STREQN(10, arg, "--fn-group") {
       fn_config* fnc;
       Char* s;
       UInt n = VG_(strtoll10)(arg+10, &s);
       if ((n <= 0) || *s != '=') return False;
       fnc = get_fnc(s+1);
       fnc->group = n;
   }

   else if VG_STREQN(18, arg, "--separate-callers") {
       fn_config* fnc;
       Char* s;
       UInt n = VG_(strtoll10)(arg+18, &s);
       if ((n <= 0) || *s != '=') return False;
       fnc = get_fnc(s+1);
       fnc->separate_callers = n;
   }

   else if VG_STREQN(15, arg, "--separate-recs") {
       fn_config* fnc;
       Char* s;
       UInt n = VG_(strtoll10)(arg+15, &s);
       if ((n <= 0) || *s != '=') return False;
       fnc = get_fnc(s+1);
       fnc->separate_recursions = n;
   }

   else if VG_STR_CLO(arg, "--callgrind-out-file", CLG_(clo).out_format) {}

   else if VG_BOOL_CLO(arg, "--mangle-names", CLG_(clo).mangle_names) {}

   else if VG_BOOL_CLO(arg, "--skip-direct-rec",
                            CLG_(clo).skip_direct_recursion) {}

   else if VG_BOOL_CLO(arg, "--dump-bbs",   CLG_(clo).dump_bbs) {}
   else if VG_BOOL_CLO(arg, "--dump-line",  CLG_(clo).dump_line) {}
   else if VG_BOOL_CLO(arg, "--dump-instr", CLG_(clo).dump_instr) {}
   else if VG_BOOL_CLO(arg, "--dump-bb",    CLG_(clo).dump_bb) {}

   else if VG_INT_CLO( arg, "--dump-every-bb", CLG_(clo).dump_every_bb) {}

   else if VG_BOOL_CLO(arg, "--collect-alloc",   CLG_(clo).collect_alloc) {}
   else if VG_BOOL_CLO(arg, "--collect-systime", CLG_(clo).collect_systime) {}
   else if VG_BOOL_CLO(arg, "--collect-bus",     CLG_(clo).collect_bus) {}
   /* for option compatibility with cachegrind */
   else if VG_BOOL_CLO(arg, "--cache-sim",       CLG_(clo).simulate_cache) {}
   /* compatibility alias, deprecated option */
   else if VG_BOOL_CLO(arg, "--simulate-cache",  CLG_(clo).simulate_cache) {}
   /* for option compatibility with cachegrind */
   else if VG_BOOL_CLO(arg, "--branch-sim",      CLG_(clo).simulate_branch) {}
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
"    --callgrind-out-file=<f>  Output file name [callgrind.out.%%p]\n"
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
"    --collect-bus=no|yes      Collect global bus events? [no]\n"
#if CLG_EXPERIMENTAL
"    --collect-alloc=no|yes    Collect memory allocation info? [no]\n"
#endif
"    --collect-systime=no|yes  Collect system call time info? [no]\n"

"\n   cost entity separation options:\n"
"    --separate-threads=no|yes Separate data per thread [no]\n"
"    --separate-callers=<n>    Separate functions by call chain length [0]\n"
"    --separate-callers<n>=<f> Separate <n> callers for function <f>\n"
"    --separate-recs=<n>       Separate function recursions up to level [2]\n"
"    --separate-recs<n>=<f>    Separate <n> recursions for function <f>\n"
"    --skip-plt=no|yes         Ignore calls to/from PLT sections? [yes]\n"
"    --skip-direct-rec=no|yes  Ignore direct recursions? [yes]\n"
"    --fn-skip=<function>      Ignore calls to/from function?\n"
#if CLG_EXPERIMENTAL
"    --fn-group<no>=<func>     Put function into separation group <no>\n"
#endif
"\n   simulation options:\n"
"    --branch-sim=no|yes       Do branch prediction simulation [no]\n"
"    --cache-sim=no|yes        Do cache simulation [no]\n"
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
  CLG_(clo).out_format       = 0;
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
  CLG_(clo).collect_bus      = False;

  CLG_(clo).skip_plt         = True;
  CLG_(clo).separate_callers = 0;
  CLG_(clo).separate_recursions = 2;
  CLG_(clo).skip_direct_recursion = False;

  /* Instrumentation */
  CLG_(clo).instrument_atstart = True;
  CLG_(clo).simulate_cache = False;
  CLG_(clo).simulate_branch = False;

  /* Call graph */
  CLG_(clo).pop_on_jump = False;

#if CLG_ENABLE_DEBUG
  CLG_(clo).verbose = 0;
  CLG_(clo).verbose_start = 0;
#endif
}
