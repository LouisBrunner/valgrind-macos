/*--------------------------------------------------------------------*/
/*--- The Omega tool: traces memory allocations and alerts when    ---*/
/*--- the final reference to an allocated block dies.              ---*/
/*---                                                     o_main.c ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Omega, a Valgrind tool for detecting memory
  leaks as they occur.

  Copyright (C) 2006-2008 Bryan "Brain Murders" Meredith 
  (A note of personal thanks to my employers at Apertio (www.apertio.com)
  for allowing the use of their time, equipment for 64bit testing and
  providing moral support.)

  Partly based upon other Valgrind tools
  Copyright (C) 2000-2008 Julian Seward, Nicholas Nethercote et al.
  jseward@acm.org
  njn@valgrind.org

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

  The current maintainer is Rich Coe <richard.coe@med.ge.com>.
*/

/*
** Read the tool documentation for an explaination of the ideas
** behind this implementation.
*/

#include "pub_tool_basics.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_hashtable.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_replacemalloc.h"
#include "pub_tool_machine.h"
#include "pub_tool_threadstate.h"
#include "pub_tool_stacktrace.h"
#include "pub_tool_options.h"
#include "pub_tool_clreq.h"

#include "coregrind/pub_core_options.h"
#include "coregrind/pub_core_debugger.h"

#include "libvex_guest_offsets.h"

#include "exp-omega.h"

/*
** A little sanity in a mad, mad world.
*/
#if !(VG_WORDSIZE == 4) && !(VG_WORDSIZE == 8)
/*
** We don't handle anything else yet.
*/
#error Unsupported VG_WORDSIZE
#endif

/*
** 4 lots of debug - always, general, memory and pbit.
** general, memory and pbit can also be turned off with a master switch.
** You wont want any of this on unless you are hacking the source around.
*/
#define NO_DEBUG(fmt, args...)
#define O_DEBUG(fmt, args...) VG_(message)(Vg_DebugMsg, fmt, ## args)

// Set to 0 to remove almost all debug from compiled tool
#if 0

static Bool o_traceMem = True; //False;
static Bool o_tracePBit = False;
static Bool o_traceGeneral = True; //False;
static Bool o_traceStop = True;

#define O_GDEBUG(fmt, args...)               \
  if(o_traceGeneral && !o_traceStop)         \
  {                                          \
    VG_(message)(Vg_DebugMsg, fmt, ## args); \
  }

#define O_MDEBUG(fmt, args...)               \
  if(o_traceMem && !o_traceStop)             \
  {                                          \
    VG_(message)(Vg_DebugMsg, fmt, ## args); \
  }

#define O_PDEBUG(fmt, args...)               \
  if(o_tracePBit && !o_traceStop)            \
  {                                          \
    VG_(message)(Vg_DebugMsg, fmt, ## args); \
  }

#define O_TRACE_ON() {o_traceStop = False;}
#define O_TRACE_OFF() {o_traceStop = True;}
#define O_TRACE_MEM_ON() {o_traceMem = True;}
#define O_TRACE_MEM_OFF() {o_traceMem = False;}
#define O_TRACE_PBIT_ON() {o_tracePBit = True;}
#define O_TRACE_PBIT_OFF() {o_tracePBit = False;}
#define O_TRACE_GENERAL_ON() {o_traceGeneral = True;}
#define O_TRACE_GENERAL_OFF() {o_traceGeneral = False;}
#define O_MASTER_DEBUG 1

/*
** Should we instrument memory loads for debugging?
** Comment out to stop register loads from showing.
*/
//#define O_TRACK_LOADS 1
#else
/*
** No debug included at all.
*/
#define O_GDEBUG(fmt, args...)
#define O_MDEBUG(fmt, args...)
#define O_PDEBUG(fmt, args...)
#define O_TRACE_ON()
#define O_TRACE_OFF()
#define O_TRACE_MEM_ON()
#define O_TRACE_MEM_OFF()
#define O_TRACE_PBIT_ON()
#define O_TRACE_PBIT_OFF()
#define O_TRACE_GENERAL_ON()
#define O_TRACE_GENERAL_OFF()

#endif

/*
** Need somewhere to give addresses to tracked pointers in registers.
** We dont write to the locations, just use their addresses.
** To make it easy to see, use the very top 64K of memory.
** Note that we might have to map this somewhere else if this is in user space.
*/
#if (VG_WORDSIZE == 4)
#define FAKE_REG_BASE 0xFFFF0000
#else
#define FAKE_REG_BASE 0xFFFFFFFFFFFF0000
#endif
#define MAP_TO_REG(tid, offset) \
          (FAKE_REG_BASE + (0x0100 * ((tid) - 1)) + (offset))
#define OFFSET_FROM_REG(regAddress) \
          ((regAddress) & 0x00ff)
#define IS_REG(addr) ((addr >= FAKE_REG_BASE) ? !0 : 0)

static UInt o_isReturnIgnoreReg(Addr reg)
{
  /*
  ** Indicate registers that are 'scratch' registers and should be ignored on
  ** function return for tracked pointer purposes.

  JRS 10 Nov 2007: Seems to me this should be somehow related to
  caller- vs callee-saved classification of registers, but not sure.
  See docs/internal/register-uses.txt for a summary.

  This fn really ought to be partitioned along VGP_arch_os lines
  rather than VGA_arch lines, since register conventions are OS
  dependant as well as CPU dependant.
  */
#if defined(VGA_x86)
  switch(OFFSET_FROM_REG(reg))
  {
  case OFFSET_x86_ECX:
  case OFFSET_x86_EDX:
    return 1;
  default:
    return 0;
  }
#elif defined(VGA_amd64)
  switch(OFFSET_FROM_REG(reg))
  {
  case OFFSET_amd64_RCX:
  case OFFSET_amd64_RSI:
  case OFFSET_amd64_RDI:
  case OFFSET_amd64_R8:
  case OFFSET_amd64_R9:
  case OFFSET_amd64_R10:
  case OFFSET_amd64_R11:
    return 1;
  default:
    return 0;
  }
#elif defined(VGA_ppc32) || defined(VGA_ppc64)
  VG_(printf)("\nOmega does not currently work on PowerPC/POWER platforms."
              "  Sorry.\n\n");
  VG_(exit)(0);
#else
#  error "Unknown arch"
#endif
   
  /*NOTREACHED*/
  tl_assert(0);
}


/*------------------------------------------------------------*/
/*--- Command Line Option Flags and Values                 ---*/
/*------------------------------------------------------------*/
/*
** Should we track all memory block allocations or just blocks
** indicated to us with the MALLOCLIKE_BLOCK user request?
*/
static Bool o_onlyMallocLike = False;
/*
** Should we show memory that leaks due to a block leaking?
*/
static Bool o_showIndirect = False;
/*
** Should we show pointers to a block that is deallocated?
*/
static Bool o_showHanging = False;
/*
** Should we show blocks with only circular references?
*/
static Bool o_showCircular = False;
/*
** Show interal stats at the end of the run.
*/
static Bool o_showInternStats = False;
/*
** Should we only show the summary report.
*/
static Bool o_showSummaryOnly = True;

/*
** Should we clear leaked blocks to try and force an error.
*/
static Bool o_poison = False;

/*
** These figures are pure wet finger in the air guestimates.
** If the user has _lots_ of memory blocks / tracked pointers, they can
** increase the prime number on the command line.
*/
/*
** Number of PBit Node entries in the hash table.
*/
static UInt o_pbitNodeHashSize = 1031;
/*
** Number of MemBlock entries in the hash table.
*/
static UInt o_memblockHashSize = 65537;
/*
** Number of Tracked Pointer entries in the hash table.
*/
static UInt o_trackedPointerHashSize = 65537;

/*------------------------------------------------------------*/
/*--- Statistics                                           ---*/
/*------------------------------------------------------------*/
typedef struct
{
  unsigned long liveTrackedPointers;
  unsigned long trackedPointersAllocated;
  unsigned long liveMemoryBlocks;
  unsigned long memoryBlocksAllocated;
  unsigned long shadowMemoryBlocksAllocated;
  unsigned long memoryBlocksLeaked;
  unsigned long memoryBlocksLostAndFound;
  unsigned long pbitNodes;
} Stats;

static Stats o_stats;

/*------------------------------------------------------------*/
/*--- PBit Tracking                                        ---*/
/*------------------------------------------------------------*/
/*
** Setup constants for PBit tracking.
*/
#if (VG_WORDSIZE == 4)
#define PBIT_MAJOR_SHIFT 7
#define PBIT_MINOR_SHIFT 2
#define PBIT_MINOR_MASK  0x1F
#elif (VG_WORDSIZE == 8)
#define PBIT_MAJOR_SHIFT 8
#define PBIT_MINOR_SHIFT 3
#define PBIT_MINOR_MASK  0x1F
#endif

/*
** Work out how many bytes a UInt of pbits covers
*/
#define PBIT_RANGE (sizeof(UInt) * 8 * VG_WORDSIZE)

/*
** Number of UInts to store in a node so that the node covers 64K
*/
#define PBIT_NODE_UINTS ((64 * 1024) / PBIT_RANGE)

/*
** Memory range covered by a pbit node
*/
#define PBIT_NODE_RANGE 0xFFFF
#define PBIT_NODE_RANGE_MASK (~PBIT_NODE_RANGE)
#define PBIT_NODE_SHIFT 16

/* Define the pbit storage node. */
typedef struct {
  VgHashNode hdr;               // Must be first item
  UInt set_bits;                // Count of set bits
  UInt pbits[PBIT_NODE_UINTS];  // 64K of coverage
} PBitNode;

/*
** We use a hash table to track the p-bits.
** The node is defined just above. The key to a node is the memory
** address right shifted PBIT_NODE_SHIFT bits.
*/
static VgHashTable o_PBits = NULL;

/*
** For speed, we keep a node to track register allocations and cache the last
** node that was accessed.
*/
static PBitNode  o_registerPBits;
static PBitNode *o_lastPBitNode = NULL;
static Addr      o_lastPBitNodeKey = 0;

/*
** Convenience macros for working out which bit in which PBIT_NODE_UINT we
** wish to address.
*/
#define PBIT_MAJOR_INDEX( addr ) \
        (((addr) & PBIT_NODE_RANGE) >> PBIT_MAJOR_SHIFT)
#define PBIT_MINOR_INDEX( addr ) \
        (((addr) >> PBIT_MINOR_SHIFT) & PBIT_MINOR_MASK)
#define PBIT_KEY( addr ) ((Addr)(addr) >> PBIT_NODE_SHIFT)

typedef struct {
  PBitNode *node;
  Addr      currentAddress;
  Addr      finalAddress;
} PBitContext;

/*
** Helper functions for doing fast searches through an address range.
*/
static Addr o_firstPBit(PBitContext *context, Addr start, SizeT length);
static Addr o_nextPBit(PBitContext *context);

/*
** Basic PBit manipulation.
*/
static PBitNode *o_getPBitNode(Addr address, Bool create)
{
  Addr key = PBIT_KEY(address);

  O_PDEBUG("o_getPBitNode(%p%s)", address,
	   create ? ", create" : "");

  O_PDEBUG("o_getPBitNode last node %p, last key %p",
	   o_lastPBitNode, o_lastPBitNodeKey);

  if(IS_REG(address))
  {
    /*
    ** This is a register - use the register PBit node.
    */
    O_PDEBUG("o_getPBitNode returning register PBit node");
    return &o_registerPBits;
  }
  else if((key == o_lastPBitNodeKey) &&
	  (o_lastPBitNode || !create))
  {
    /*
    ** This is in the same node as last time.
    */
    O_PDEBUG("o_getPBitNode returning last PBit node");
    return o_lastPBitNode;
  }
  else
  {
    /*
    ** It's a new node.
    ** Look it up then cache both the node and the node key.
    */
    o_lastPBitNode = VG_(HT_lookup)(o_PBits, key);
    o_lastPBitNodeKey = key;

    if(!o_lastPBitNode & create)
    {
      /*
      ** We don't have a node for this address. Create one now.
      */
      o_lastPBitNode = VG_(malloc)( "om.ogPBN.1", sizeof(PBitNode) );
      tl_assert(o_lastPBitNode);
      VG_(memset)(o_lastPBitNode, 0, sizeof(PBitNode));
      o_lastPBitNode->hdr.key = key;

      /*
      ** Add this node into the hash table.
      */
      VG_(HT_add_node)(o_PBits, o_lastPBitNode);
      
      O_PDEBUG("Created PBit node beginning %p for address %p",
	       (key << PBIT_NODE_SHIFT),
	       address);

      o_stats.pbitNodes++;
      
    }
    O_PDEBUG("o_getPBitNode returning lookup PBit node");

    return o_lastPBitNode;
  }
}

static void o_setPBit( Addr address )
{
  /*
  ** Retrieve the node that contains this address then set the appropriate bit.
  */
  PBitNode *pbn = o_getPBitNode(address, True);

  O_PDEBUG("o_setPBit(%p)", address);

  O_PDEBUG("o_setPBit - node = %p, MAJOR = %d, MINOR = %d",
	   pbn,
	   PBIT_MAJOR_INDEX(address),
	   PBIT_MINOR_INDEX(address));
  /*
  ** The PBit might not be clear so only tweak things if it is.
  */
  if(!(pbn->pbits[PBIT_MAJOR_INDEX(address)] &
       (1 << PBIT_MINOR_INDEX(address))))
  {
    /*
    ** Set the pbit and increment the convenience count.
    */
    pbn->pbits[PBIT_MAJOR_INDEX(address)] |=
      (1 << PBIT_MINOR_INDEX(address));
    pbn->set_bits++;
  }
  
  O_PDEBUG("o_setPBit done");
  return;
}

static void o_clearPBit( Addr address )
{
  /*
  ** Retrieve the node that contains this address. If the node does not exist,
  ** we assert as this really shouldnt happen.
  */
  PBitNode *pbn = o_getPBitNode(address, False);

  O_PDEBUG("o_clearPBit(%p)", address);

  tl_assert(pbn);

  /*
  ** The PBit might not be set so only tweak things if it is.
  */
  if(pbn->pbits[PBIT_MAJOR_INDEX(address)] &
     (1 << PBIT_MINOR_INDEX(address)))
  {
    /*
    ** Clear the pbit and decrement the convenience count.
    */
    pbn->pbits[PBIT_MAJOR_INDEX(address)] &=
      ~(1 << PBIT_MINOR_INDEX(address));
    pbn->set_bits--;
  }

  return;
}

static Bool o_isPBitSet( Addr address )
{
  /*
  ** Retrieve the node that contains this address. If the node does not exist,
  ** the Pbit isnt set ;-)
  */
  PBitNode *pbn = o_getPBitNode(address, False);

  O_PDEBUG("o_isPBitSet(%p)", address);

  if(!pbn)
    return 0;

  /*
  ** Return the Pbit status.
  */
  return ((pbn->pbits[PBIT_MAJOR_INDEX(address)] &
	   (1 << PBIT_MINOR_INDEX(address))) != 0);
}

/*
** For ease of range checking PBits, we provide the following two functions.
** The idea is that you call the first one with your start address and range.
** It returns the first address that is marked by a PBit or 0 if the range is
** clear (we overlap the supplied range in order to check partial pointers at
** each end). By calling the second one with the same context until it returns
** zero, you get all of the PBits within the range. You supply the context so
** we should be able to nest calls if need be.
*/
static Addr o_firstPBit(PBitContext *context, Addr start, SizeT length)
{
  const Addr MASK = ~(VG_WORDSIZE - 1);

  tl_assert(context);
  tl_assert(start > VG_WORDSIZE);

  O_PDEBUG("o_firstPBit(%p, %p)", start, length);
  /*
  ** Optimisation for single pointer ranges and bizarre 0 length calls.
  */
  if(!length)
  {
    return 0;
  }
  else if(length <= VG_WORDSIZE)
  {
    /*
    ** Set the current address to 0.
    */
    context->currentAddress = 0;
    return (o_isPBitSet(start)) ? (start & MASK) : 0;
  }

  /*
  ** Setup the current and final addresses. Note that we set the current
  ** address to one aligned address below because of how nextPBit works.
  */
  context->currentAddress = ((start & MASK) - VG_WORDSIZE);
  context->finalAddress = ((start + length - 1) & MASK);
  
  context->node = o_getPBitNode(context->currentAddress, False);

  O_PDEBUG("o_firstPBit current %p, final %p",
	   context->currentAddress, context->finalAddress);

  return o_nextPBit(context);
}

static Addr o_nextPBit(PBitContext *context)
{
  /*
  ** Current address is the last address we returned.
  ** We keep going until we have checked final address.
  */
  UInt pbits;
  Addr startAddr;
  Addr foundAddr = 0;
  UInt majorIndex;
  UInt minorIndex;
  
  tl_assert(context);

  /*
  ** When the current address is set to 0, we just exit.
  */
  if(context->currentAddress == 0)
  {
    return 0;
  }

  O_PDEBUG("o_nextPBit(%p,%p)",
	   context->currentAddress, context->finalAddress);

  while(!foundAddr &&
	(context->currentAddress <= context->finalAddress))
  {
    /*
    ** Check if we need another node and get it if we do.
    */
    startAddr = context->currentAddress + VG_WORDSIZE;

    O_PDEBUG("o_nextPBit c %p s %p", context->currentAddress, startAddr);

    if(PBIT_KEY(context->currentAddress) !=
       PBIT_KEY(startAddr))
    {
      O_PDEBUG("o_nextPBit getting next node %p",
	       startAddr & PBIT_NODE_RANGE_MASK);

      context->node = o_getPBitNode(startAddr, False);
    }
    context->currentAddress = startAddr;

    /*
    ** Check if we have a node - skip to next node (final address
    ** permitting) if we dont. This is the 64k of addresses at a time
    ** comparison.
    */
    if(!context->node)
    {
      O_PDEBUG("o_nextPbit: no node.");

      if(context->currentAddress > context->finalAddress)
      {
	/*
	** We have passed the final address - time to stop looking.
	*/
	O_PDEBUG("o_nextPbit: current > final");
	continue;
      }
      else if((context->currentAddress & PBIT_NODE_RANGE_MASK) !=
	      (context->finalAddress & PBIT_NODE_RANGE_MASK))
      {
	/*
	** Align to VG_WORDSIZE below the next node range then loop.
	*/
	O_PDEBUG("o_nextPbit: aligning to next node. (%p, %p)",
		 context->currentAddress,
		 context->finalAddress);

	context->currentAddress += (PBIT_NODE_RANGE + 1);
	context->currentAddress &= PBIT_NODE_RANGE_MASK;
	context->currentAddress -= VG_WORDSIZE;

	O_PDEBUG("o_nextPbit: aligned to %p",
		 context->currentAddress);

	continue;
      }
      else
      {
	/*
	** Node range is the same but no node == no pbits.
	*/
	context->currentAddress = context->finalAddress + VG_WORDSIZE;
	break;
      }
    }
    
    /*
    ** The index of the PBit array item we want to check then get the pbits.
    */
    majorIndex = PBIT_MAJOR_INDEX(context->currentAddress);
    minorIndex = PBIT_MINOR_INDEX(context->currentAddress);
    pbits = context->node->pbits[majorIndex];

    /*
    ** Mask off addresses below the current address then test.
    */
    pbits &= ~((1 << minorIndex) - 1);

    O_PDEBUG("o_nextPbit: major %d, minor %d, bit %p",
	     majorIndex, minorIndex, pbits);
    /*
    ** This checks up to PBIT_RANGE at a time (256 addresses on a
    ** 64bit machine).
    */
    if(!pbits)
    {
      /*
      ** No pbits set in this UInt. Set the current address to VG_WORDSIZE
      ** below the next UInt then loop around.
      */
      context->currentAddress += PBIT_RANGE;
      context->currentAddress &= ~(PBIT_RANGE - 1);
      context->currentAddress -= VG_WORDSIZE;
      
      continue;
    }

    /*
    ** Now we walk the UInt a bit at a time.
    */
    for(;
	((minorIndex <= PBIT_MINOR_MASK) &&
	 (context->currentAddress <= context->finalAddress))
	  ; minorIndex++)
    {
      if(pbits & (1 << minorIndex))
      {
	/*
	** We have a match.
	*/
	foundAddr = context->currentAddress;
	O_PDEBUG("o_nextPbit found %p", foundAddr);
	break;
      }
      else
      {
	context->currentAddress += VG_WORDSIZE;
      }
    }
  }

  /*
  ** Final range check.
  */
  if(foundAddr > context->finalAddress)
  {
    foundAddr = 0;
  }

  /*
  ** Store the result so that we know where to start from next time.
  */
  context->currentAddress = foundAddr;

  O_PDEBUG("o_nextPbit returning %p", foundAddr);

  return foundAddr;
}

/*------------------------------------------------------------*/
/*--- Error Report and Suppression Tracking                ---*/
/*------------------------------------------------------------*/
/*
** We hold a doubley linked list of Exe contexts for leaks and suppressions.
** If a block is tagged as leaked then comes back to life, we move it
** into the suppression list. We always check the suppression list first
** before adding a record to the leaked list.
** We keep a count of how may times a record matches as it saves space.
*/
struct _BlockRecord {
  struct _BlockRecord *next;
  struct _BlockRecord *prev;
  ExeContext          *allocated;
  ExeContext          *leaked;
  UInt                 bytes;
  SizeT                count;
};

typedef struct _BlockRecord BlockRecord;

typedef struct {
  BlockRecord *start;
  BlockRecord *end;
} BlockRecordList;
static BlockRecordList o_leakRecords = {NULL, NULL};
static BlockRecordList o_suppressionRecords = {NULL, NULL};

#define DUMP_BLOCK(block) \
  O_DEBUG("n %p, p %p, a %p, l %p, c %d b %p", \
          block->next, block->prev, \
          block->allocated, block->leaked, block->count, \
          block->bytes);

/*
** List handling - we need to be able to add and remove a single block
** from anywhere in the list but the chances are, removals will come from
** the end, hence using a doubly linked list. We also need to walk the list
** to find a matching item. Again, we do this backwards as it tends to get
** a match faster in the case of moving newly leaked block records into
** the suppression list.
*/
static void o_addBlockRecord(BlockRecordList *list, BlockRecord *item)
{
  /*
  ** Catch start case.
  */
  tl_assert(list && item);

  NO_DEBUG("o_addBlockRecord pre()");
  //DUMP_BLOCK(item);

  if(!list->start)
  {
    list->start = list->end = item;
    item->prev = item->next = NULL;
  }
  else
  {
    /*
    ** OK, add it onto the end.
    */
    item->prev = list->end;
    item->next = NULL;
    list->end->next = item;
    list->end = item;
  }
  NO_DEBUG("o_addBlockRecord post()");
  //DUMP_BLOCK(item);
  return;
}

static void o_removeBlockRecord(BlockRecordList *list, BlockRecord *item)
{
  /*
  ** We don't check that the item is in the list.
  ** Ensure you check with the findBlockRecord function.
  */
  tl_assert(list && item);

  NO_DEBUG("o_removeBlockRecord pre()");
  //DUMP_BLOCK(item);
  if(item->prev)
  {
    /*
    ** Not at the start.
    */
    item->prev->next = item->next;
  }
  else
  {
    /*
    ** At the start.
    */
    list->start = item->next;
  }

  if(item->next)
  {
    /*
    ** Not at the end.
    */
    item->next->prev = item->prev;
  }
  else
  {
    /*
    ** At the end.
    */
    list->end = item->prev;
  }

  NO_DEBUG("o_removeBlockRecord post()");
  //DUMP_BLOCK(item);

  return;
}

static BlockRecord *o_findBlockRecord(BlockRecordList *list,
				      ExeContext      *allocated,
				      ExeContext      *leaked)

{
  /*
  ** Search backwards for the block record that matches the contexts.
  ** We allow leaked to be null so that we can handle the circular checking
  ** blocks as well which only have an allocated context.
  */
  BlockRecord *item = NULL;

  tl_assert(list && allocated);

  item = list->end;

  while(item)
  {
    if(VG_(eq_ExeContext)(Vg_HighRes, item->allocated, allocated) &&
       ((!item->leaked && !leaked) ||
	((item->leaked && leaked) &&
	 VG_(eq_ExeContext)(Vg_HighRes, item->leaked, leaked))))
    {
      break;
    }

    item = item->prev;
  }

  return item;
}

static Bool o_addLeakedBlock(ExeContext *allocated,
			     ExeContext *leaked,
			     SizeT       size)
{
  BlockRecord *item = NULL;

  tl_assert(allocated && leaked);

  /*
  ** See if we already have this block.
  ** Check the suppression record first.
  */
  item = o_findBlockRecord(&o_suppressionRecords, allocated, leaked);

  if(!item)
  {
    /*
    ** Not in the suppression record.
    ** Try the leaked block list.
    */
    item = o_findBlockRecord(&o_leakRecords, allocated, leaked);
  }

  if(item)
  {
    /*
    ** Just increment the count.
    */
    item->count++;
    item->bytes += size;
    //O_DEBUG("o_addLeakedBlock - block exists");
    //DUMP_BLOCK(item);
    return False;
  }
  else
  {
    /*
    ** Create a new block and add it to the leaked list.
    */
    item = VG_(malloc)("om.oaLB.1", sizeof(BlockRecord));
    tl_assert(item);
    
    item->count = 1;
    item->bytes = size;
    item->next = item->prev = NULL;
    item->allocated = allocated;
    item->leaked = leaked;

    o_addBlockRecord(&o_leakRecords, item);

    return True;
  }
  
}

static Bool o_addSuppressionBlock(ExeContext *allocated,
				  ExeContext *leaked)
{
  BlockRecord *item = NULL;

  tl_assert(allocated && leaked);

  /*
  ** See if we already have this block.
  ** Check the suppression record first.
  */
  item = o_findBlockRecord(&o_suppressionRecords, allocated, leaked);

  if(!item)
  {
    /*
    ** Not in the suppression record.
    ** Try the leaked block list.
    */
    item = o_findBlockRecord(&o_leakRecords, allocated, leaked);

    if(!item)
    {
      VG_(tool_panic)("suppressing block that didnt leak :-(");
    }
    else
    {
      /*
      ** Move the block to the suppression list.
      */
      o_removeBlockRecord(&o_leakRecords, item);
      o_addBlockRecord(&o_suppressionRecords, item);
    }
  }
  else
  {
    /*
    ** The block is already suppressed - just increase the count.
    */
    item->count++;

    //O_DEBUG("o_addSuppressionBlock - block exists");
    //DUMP_BLOCK(item);
    return False;
  }

  return True;
}

/*------------------------------------------------------------*/
/*--- Allocated Block and Pointer Tracking                 ---*/
/*------------------------------------------------------------*/
/*
** Where these structures have address references, they are the address
** of the item in client memory NOT the address of either of these
** internal tracking structures.
*/
struct _MemBlock;
typedef struct {
  VgHashNode        hdr;      // Must be first item
  Addr              block;    // Address of the allocated block start
  SizeT             length;   // Length of the allocated block
  struct _MemBlock *memBlock; // Pointer to the memblock
} TrackedPointer;

typedef struct _MemBlock {
  VgHashNode      hdr;       // Must be first item
  SizeT           length;    // Length of the allocated block
  ExeContext     *where;     // Where the block was allocated
  UInt            refNum;    // Number of back references
  TrackedPointer **pointers; // Back references to TrackedPointer info
  struct _MemBlock *shadowing; // Set to memblock of block that we shadow
  struct _MemBlock *shadowed;  // Set to memblock of our shadow
  ExeContext     *leaked;    // Where we think the block leaked
  UInt            nonRegCount; // Non register tracked pointers
  Int             external;  // Used in circular dependency checking

  TrackedPointer *maybeLast; // Last live tracked pointer on function return
  ExeContext     *funcEnd;   // matching exe context for the end of the function
  Bool            doLeak;    // Set if this block should leak on instruction
                             // end. We have to make instructions atomic or we
                             // go bang on things like xchng as there is no way
                             // of telling which value gets overwritten first.
  struct _MemBlock *next;    // Linked list of blocks that might be leaking at
                             // instruction end.
  int              depth;    // Depth that the potential leak occurred at.
  TrackedPointer  *wasLast;  // Pointer t

  UInt             nonScratch; // Number of non-scratch registers.
} MemBlock; 

/*
** Shadows?
** This helps to solve the problem of where a program does its own memory
** management of the kind:

1   secret *foo = malloc(sizeof(bar) + sizeof(secret) + alignment_correction);
2   foo->secret_stuff = magic_key;
3       etc.
4   foo++;
5   return (bar*)foo;

** If the pointer to foo is shadowed at some internal offset to the block
** start, we create a shadow record and link it to the main block so that
** we can track references to either. Without this we do a leak alert at
** line 4 instead which is undesireable.
**
** There can only be one shadow to a block unless we need more and someone
** wants to code it. A side effect of the current implementation allows a
** shadow of a shadow but it is explicitly blocked for now.
*/

/*
** We use separate hash tables to track the pointers and allocated blocks.
** The key of each node is the address of the corresponding item in client
** memory, shifted right to remove the wasted bits caused by alignment of
** pointers in memory.
*/
#if (VG_WORDSIZE == 4)
#define TRACK_MINOR_SHIFT 2
#define TRACK_MINOR_MASK ~0x03
#elif (VG_WORDSIZE == 8)
#define TRACK_MINOR_SHIFT 3
#define TRACK_MINOR_MASK ~0x07
#endif

#define TRACKED_KEY( a ) ((UWord)(a) >> TRACK_MINOR_SHIFT)
#define FROM_TRACKED_KEY( a ) ((UWord)(a) << TRACK_MINOR_SHIFT)

/*
** Storage for the two hash tables we need.
*/
static VgHashTable o_MemBlocks = NULL;
static VgHashTable o_TrackedPointers = NULL;

/*
** Start of a linked list of blocks that may be leaking during this original
** processor instruction. Instructions are broken down inside VEX so a single
** original instruction can become many VEX instructions. By not doing leak
** reports until the end of the original instruction, everything becomes
** atomic again - the stack moves and the popped value appears in the register
** in one movement rather than two which cause a leak if the stack is
** invalidated before the value appears in the register. xchng works both ways
** around and so on.
*/
static MemBlock *doLeakList      = NULL;
static UInt      doLeakListCount = 0;
static Bool      doLeakNow       = False;

/*
** Set when we are removing pointers within a free()ed block.
*/
static Bool o_clearingBlock = False;

/*
** Set when we are removing pointers within a free()ed block or a
** block that leaked. It shows the indirection level in cascades.
*/
static UInt        o_indirectChecking = 0;
static ExeContext *o_indirectStack = NULL;

/*
** Set when the stack is unwinding.
*/
static Bool o_stackUnwind = False;

static void o_killRange(Addr start, SizeT length);

/*
** This is set to stop us from tracking leaks once we exit main.
** (May well need a per thread flag to catch when threads exit as well.)
*/
static Bool o_inhibitLeakDetect = False;


static void o_cleanupTrackedPointers( MemBlock * mb )
{
  UInt pointerIndex;

  for(pointerIndex = 0; pointerIndex < mb->refNum; pointerIndex++)
  {
    TrackedPointer *p =
      VG_(HT_remove)(o_TrackedPointers,
		     mb->pointers[pointerIndex]->hdr.key);
    
    tl_assert(p);
    O_GDEBUG("Removing tracked pointer at %p pointing to %p",
	     FROM_TRACKED_KEY(p->hdr.key),
	     mb->hdr.key);

    /*
    ** Remove the PBit for this tracked pointer.
    */
    o_clearPBit(FROM_TRACKED_KEY(p->hdr.key));
    
    /*
    ** Show any pointers to this block as we deallocate them.
    */
    if(o_showHanging)
    {
      if(IS_REG(FROM_TRACKED_KEY(p->hdr.key)))
      {
	/*
	** Maybe decode registers to names later?
	*/
	O_DEBUG("Removing hanging pointer in a register to block %p",
		(void*)(p->block));
      }
      else
      {
	O_DEBUG("Removing hanging pointer at %p to block %p",
		(void*)(FROM_TRACKED_KEY(p->hdr.key)),
		(void*)(p->block));
      }
    }
    VG_(free)(p);
    o_stats.liveTrackedPointers--;
  }

  /*
  ** Free off the pointers back reference.
  */
  VG_(free)(mb->pointers);
  mb->pointers = NULL;
  mb->refNum = 0;

  return;
}

static void o_cleanupMemBlock( MemBlock **mbpp )
{
  MemBlock *mb;

  O_GDEBUG("o_cleanupMemBlock(%p)", mbpp);
  /*
  ** Sanity check.
  */
  if(!mbpp || !*mbpp)
  {
    O_DEBUG("o_cleanupMemBlock passed null memory block pointer.");
    return;
  }

  /*
  ** Take a local copy with less indirection.
  */
  mb = *mbpp;

  O_GDEBUG("o_cleanupMemBlock mb=%p", mb->hdr.key);

  /*
  ** If this is a shadowed block, complain then return.
  */
  if(mb->shadowing)
  {
    O_DEBUG("Trying to cleanup a shadow block at %p tracking %p",
            (void*)(mb->hdr.key),
            (void*)(mb->shadowing->hdr.key));
    return;
  }

  /*
  ** If a shadow exists, clean it up.
  */
  if(mb->shadowed)
  {
    MemBlock *shadowed = mb->shadowed;

    /*
    ** Cleanup its pointers, remove it from the hash table then
    ** free off the block.
    */
    O_GDEBUG("cleanup shadow pointers");
    o_cleanupTrackedPointers(shadowed);
    (void)VG_(HT_remove)(o_MemBlocks, shadowed->hdr.key);
    VG_(free)(shadowed);

    o_stats.liveMemoryBlocks--;
  }

  /*
  ** Free off the tracked pointers.
  */
  O_GDEBUG("cleanup tracked pointers");
  o_cleanupTrackedPointers(mb);

  /*
  ** Check for tracked pointers inside the allocated block being lost.
  */
  o_indirectChecking++;
  o_clearingBlock = True;
  o_killRange(mb->hdr.key,
	      mb->length);
  o_clearingBlock = False;
  o_indirectChecking--;

  /*
  ** Now free off the memory block. 
  */
  VG_(free)(mb);
  o_stats.liveMemoryBlocks--;

  /*
  ** Clear the passed in pointer.
  */
  *mbpp = NULL;

  return;
}

static void o_addMemBlockReference( MemBlock *mb, TrackedPointer *tp )
{
  MemBlock *smb = mb;

  O_GDEBUG("o_addMemBlockReference tp=%p, mb=%p",
	   FROM_TRACKED_KEY(tp->hdr.key),
	   mb->hdr.key);

  /*
  ** Check if we are shadowing.
  */
  if(mb->shadowing)
  {
    /*
    ** Get the mem block for the true allocated block.
    ** Note that this leaves smb pointing to the shadow block which is
    ** what we want.
    */
    mb = mb->shadowing;
  }

  /*
  ** Check if the block previously leaked.
  */
  if(!mb->shadowed && !mb->refNum && mb->leaked)
  {
    /*
    ** Seems that the block didnt leak after all.
    */
    if(o_addSuppressionBlock(mb->where, mb->leaked) && !o_showSummaryOnly)
    {
      O_DEBUG("Welcome back to the supposedly leaked block at %p. Illegal read?",
	      (void*)(mb->hdr.key));

      VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(), VG_(clo_backtrace_size));
      O_DEBUG("");
    }
    
    mb->leaked = NULL;
    o_stats.memoryBlocksLeaked--;
    o_stats.memoryBlocksLostAndFound++;
  }

  /*
  ** Populate the tracked pointer then add it to the hash.
  ** We use the shadow block so that it points to the correct place.
  ** Add the back reference to the mem block.
  */
  tp->block = smb->hdr.key;
  tp->length = mb->length;
  tp->memBlock = smb;
  VG_(HT_add_node)(o_TrackedPointers, tp);

  /*
  ** Do we need more memory for pointers?
  */
  if(!smb->pointers)
  {
    smb->pointers =
      VG_(malloc)("om.oAMBR.1", (smb->refNum + 8) * sizeof(TrackedPointer *));
    tl_assert(smb->pointers);
  }
  else if(!((smb->refNum + 1) & 7))
  {
    /*
    ** Add space for another 8 back references.
    ** Note that this will also shrink us if needed.
    */
    smb->pointers =
      VG_(realloc)("om.oAMBR.2",
                   smb->pointers, ((smb->refNum + 8) * sizeof(Addr)));
    tl_assert(smb->pointers);
  }

  smb->pointers[smb->refNum] = tp;

  /*
  ** Track register and memory pointers.
  */
  if(!IS_REG(FROM_TRACKED_KEY(smb->pointers[smb->refNum]->hdr.key)))
  {
    smb->nonRegCount++;
  }
  else if(!o_isReturnIgnoreReg(FROM_TRACKED_KEY(smb->pointers[smb->refNum]->hdr.key)))
  {
    smb->nonScratch++;
  }

  /*
  ** Clear the maybeLast and funcEnd. Adding a reference means that
  ** the cached one wasnt the last.
  */
  smb->maybeLast = NULL;
  smb->funcEnd = NULL;

  /*
  ** Clear the doLeak flag - we just added a reference so the block survived
  ** the instruction.
  */
  smb->doLeak = False;

  smb->refNum++;
  O_MDEBUG("Added tracked pointer at %p pointing to %s%p",
	   FROM_TRACKED_KEY(tp->hdr.key),
	   smb->shadowing ? "(S)" : "",
	   smb->hdr.key);

  return;
}

static void o_removePointerFromList(MemBlock *mb, TrackedPointer *tp)
{
  UInt pointerNum;

  O_GDEBUG("removePointerFromList tp=%p mb=%p",
	   FROM_TRACKED_KEY(tp->hdr.key),
	   mb->hdr.key);

  /*
  ** Check that this tracked pointer belongs to this block.
  */
  tl_assert(tp->memBlock == mb);

  /*
  ** Find the tracked pointer in the memory blocks' list.
  */
  for(pointerNum = 0; pointerNum < mb->refNum; pointerNum++)
  {
    if(mb->pointers[pointerNum] == tp)
    {
      /*
      ** Found it.
      ** If this is not the last pointer in the list, copy the last
      ** one over it.
      */
      if((pointerNum + 1) != mb->refNum)
      {
	mb->pointers[pointerNum] = mb->pointers[(mb->refNum - 1)];
      }
      
      break;
    }
  }

  /*
  ** Track register and memory pointers.
  */
  if(!IS_REG(FROM_TRACKED_KEY(tp->hdr.key)))
  {
    mb->nonRegCount--;
  }
  else if(!o_isReturnIgnoreReg(FROM_TRACKED_KEY(tp->hdr.key)))
  {
    mb->nonScratch--;
  }

  mb->refNum--;

  return;
}

static void o_doLeakReport(MemBlock *mb);
static void o_removeMemBlockReference( MemBlock *mb, TrackedPointer *tp )
{
  MemBlock *smb = NULL;
  SizeT     refCount = 0;
  UInt      nonRegCount = 0;
  Bool      shadowed = False;

  /*
  ** We need the tracked pointer object.
  */
  tl_assert(tp);

  /*
  ** If we dont have the memory block, get it from the tracked pointer.
  */
  if(!mb)
  {
    mb = tp->memBlock;
  }
  tl_assert(mb);

  O_GDEBUG("o_removeMemBlockReference tp=%p, mb=%p",
	   FROM_TRACKED_KEY(tp->hdr.key),
	   mb->hdr.key);

  smb = mb;
  refCount = smb->refNum;
  nonRegCount = smb->nonRegCount;

  O_GDEBUG("(A)refCount %d, o_stackUnwind %c, nonRegCount %d, isReg %c",
	  refCount,
	  (o_stackUnwind ? 'Y' : 'N'),
	  nonRegCount,
	  IS_REG(FROM_TRACKED_KEY(tp->hdr.key)) ? 'Y' : 'N');

  /*
  ** Check if we are shadowing.
  */
  if(mb->shadowing)
  {
    /*
    ** Get the mem block for the true allocated block.
    ** Note that this leaves smb pointing to the shadow which is correct.
    */
    mb = mb->shadowing;
#if defined(O_MASTER_DEBUG)
    if(!o_traceStop)
    {
      int count;
      for(count = 0; count < mb->refNum && count < 6; count++)
	O_GDEBUG("  %p", FROM_TRACKED_KEY(mb->pointers[count]->hdr.key));
    }
#endif
    refCount += mb->refNum;
    shadowed = True;
    nonRegCount += mb->nonRegCount;
  }
  else if(mb->shadowed)
  {
    /*
    ** Get the mem block for the shadow as we need the refNum from it.
    */
    MemBlock *tmb = mb->shadowed;
#if defined(O_MASTER_DEBUG)
    if(!o_traceStop)
    {
      int count;
      for(count = 0; count < tmb->refNum && count < 6; count++)
        O_GDEBUG("  %p", FROM_TRACKED_KEY(tmb->pointers[count]->hdr.key));
    }
#endif
    refCount += tmb->refNum;
    shadowed = True;
    nonRegCount += tmb->nonRegCount;
  }
#if defined(O_MASTER_DEBUG)
  else if(!o_traceStop)
  {
    int count;
    for(count = 0; count < mb->refNum && count < 6; count++)
      O_GDEBUG("  %p", FROM_TRACKED_KEY(mb->pointers[count]->hdr.key));

  }
#endif

  O_GDEBUG("(B)rCnt %d, nRCnt %d, ns %d, shad %c, free %c",
	  refCount,
	  nonRegCount,
           mb->nonScratch,
	  (shadowed ? 'Y' : 'N'),
          (o_clearingBlock ? 'Y' : 'N'));
  /*
  ** We really should have at least one tracked pointer.
  */
  tl_assert(refCount);

#if defined(O_MASTER_DEBUG)
  if(!o_traceStop)
  {
    VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(), 8);O_DEBUG("");
  }
#endif

  /*
  ** We remove the tracked pointer from the hash table but do not delete it.
  ** This allows a slight gain where a tracked pointer can immediately be
  ** reused rather than free()ed off and a new one malloc()ed.
  ** We then remove the back reference from the memory block and
  ** squeal if it is the last one. We don't clean the tracked pointer as this
  ** is a waste if it is going to be free()ed off.
  ** If warn indirect is set and this is an indirect check, do nothing.
  */
  (void)VG_(HT_remove)(o_TrackedPointers, tp->hdr.key);

  O_GDEBUG("Removing tracked pointer at %p pointing to %p",
	  FROM_TRACKED_KEY(tp->hdr.key),
	  smb->hdr.key);

  if((refCount <= 1) // Last pointer

     /*
     ** Catch cascades of memory blocks when we call free().
     */
     || (o_clearingBlock && !shadowed && !mb->nonScratch &&
	 (nonRegCount == 1) && !IS_REG(FROM_TRACKED_KEY(tp->hdr.key)))

#if defined(VGA_x86)
     /*
     ** Losing all in memory pointers within a basic block is not a good sign.
     */
     || (!o_stackUnwind && (nonRegCount == 1) &&
	 !IS_REG(FROM_TRACKED_KEY(tp->hdr.key)))
#endif
    )
  {
    if((!o_inhibitLeakDetect)
       /*
       ** Don't report when there are just register based pointers left and
       ** we have already reported the block as leaked.
       */
       && !(mb->leaked && IS_REG(FROM_TRACKED_KEY(tp->hdr.key)))
      )
    {
      /*
      ** Set the doLeak flag for the block and add it to the doLeakList.
      ** We also need to stash the indirect depth value for possibly reporting
      ** later. Finally, if maybeLast matches the pointer that is being removed
      ** and thus causing the leak, we leave maybeLast and funcEnd otherwise, we
      ** zero them.
      */
      mb->depth = o_indirectChecking;
      if(mb->maybeLast != tp)
      {
	mb->maybeLast = NULL;
	mb->funcEnd = NULL;
      }

      /*
      ** Cascades triggered by a doLeak being actioned should report
      ** immediately, rather than being added to the doLeakList. Likewise
      ** cascades caused by freeing a block.
      */
      if(doLeakNow || o_clearingBlock)
      {
	o_doLeakReport(mb);
      }
      else
      {
	mb->doLeak = True;
	mb->next = doLeakList;
	doLeakList = mb;
	doLeakListCount++;
      }
    }
  }

  /*
  ** Finally, remove the pointer from the blocks' list.
  */
  o_removePointerFromList(smb, tp);

  return;
}

static void o_doLeakReport(MemBlock *mb)
{
  Bool doReport = True;

  if(mb->maybeLast)
  {
    // This is the suspected last pointer - use the cached stacktrace
    O_MDEBUG("maybe last was the last");
    tl_assert(mb->funcEnd);
    mb->leaked = mb->funcEnd;
    o_indirectStack = mb->funcEnd;
  }
  else if(mb->depth && o_indirectStack)
  {
    O_MDEBUG("indirect with indirect stack set");
    // We are cascading - use the cached stacktrace, if there is one
    mb->leaked = o_indirectStack;
  }
  else
  {
    O_MDEBUG("creating new context maybeLast=0");
    // Get the current stacktrace
    mb->leaked = VG_(record_ExeContext)(VG_(get_running_tid)(),
                                        0/*first_ip_delta*/);
  }

  doReport = o_addLeakedBlock(mb->where, mb->leaked, mb->length);
  /*
  ** Report the probable leak.
  */
  o_stats.memoryBlocksLeaked++;
  
  if(doReport && !o_showSummaryOnly)
  {
    if(mb->depth)
    {
      if(o_showIndirect)
      {
	VG_(message)(Vg_UserMsg,
		     "Probably indirectly (level %d) leaking block of %ld(0x%lx) bytes",
		     mb->depth,
		     mb->length,
		     mb->length);
      }
    }
    else
    {
      VG_(message)(Vg_UserMsg,
		   "Probably leaking block of %ld(0x%lx) bytes",
		   mb->length,
		   mb->length);
    }
    
    if(!mb->depth || o_showIndirect)
    {
      VG_(pp_ExeContext)(mb->leaked);
      
      VG_(message)(Vg_UserMsg,
		   " Block at %#lx allocated", mb->hdr.key);
      VG_(pp_ExeContext)(mb->where);
      VG_(message)(Vg_UserMsg,"");
    }
    
    /*
    ** Only attach the debugger for the first leaking block in the chain
    ** and only when show summary is disabled (--instant-reports).
    */
    if(!mb->depth && VG_(clo_db_attach))
    {
      VG_(start_debugger)(VG_(get_running_tid)());
    }
  }

  /*
  ** Check for tracked pointers inside the allocated block being lost.
  */
  o_indirectChecking++;
  o_killRange(mb->hdr.key, mb->length);
  o_indirectChecking--;

  /*
  ** Poison the block if requested.
  */
  if(o_poison)
    VG_(memset)((Addr *)mb->hdr.key, 0, mb->length);

  return;
}

static Bool o_setupShadow(TrackedPointer *tp, Addr address)
{
  Bool doneShadow = False;
  MemBlock *mb = NULL;
  MemBlock *smb = NULL;

  O_MDEBUG("setup shadow tp %p block %p address %p",
	   FROM_TRACKED_KEY(tp->hdr.key), tp->block, address);
  /*
  ** Get the memory block for the tracked pointer.
  ** It should exist.
  */
  mb = tp->memBlock;
  tl_assert(mb);

  /*
  ** If this is a shadow block, get the main block as well.
  ** It should exist.
  */
  smb = mb;
  if(mb->shadowing)
  {
    mb = mb->shadowing;
    tl_assert(mb);
  }

  /*
  ** If the block is already shadowed at address, bail out and let the
  ** normal code handle it.
  */
  if(mb->shadowed)
  {
    if(mb->shadowed->hdr.key == address)
    {
      O_MDEBUG("already shadowed %p", address);
      return False;
    }
    /*
    ** Get the shadow block.
    */
    smb = mb->shadowed;
    tl_assert(smb);
  }
  
  /*
  ** Check if address is within the block that we are tracking.
  ** If it is then we need to work out whether to create a
  ** new shadow or move an eixsting one.
  */
  if((address > mb->hdr.key) &&
     (address < (mb->hdr.key + mb->length)))
  {
    doneShadow = True;

    O_MDEBUG("About to shadow internal address %p to block %p in %p",
	     address,
	     mb->hdr.key,
	     FROM_TRACKED_KEY(tp->hdr.key));

    if(smb == mb)
    {
      O_MDEBUG("creating new shadow");
      /*
      ** Create a new shadow for the block.
      */
      smb = VG_(malloc)( "om.osuS.1", sizeof(MemBlock) );
      tl_assert(smb);

      o_stats.shadowMemoryBlocksAllocated++;
      o_stats.liveMemoryBlocks++;

      VG_(memset)(smb, 0, sizeof(MemBlock));
      smb->hdr.key = address;
      smb->length = 0;
      smb->where = 0; // Dont need this in the shadow.
      smb->shadowing = mb;
      mb->shadowed = smb;
      VG_(HT_add_node(o_MemBlocks, smb));

      /*
      ** Move the tracked pointer from the main block to the shadow.
      */
      (void)VG_(HT_remove)(o_TrackedPointers, tp->hdr.key);
      o_removePointerFromList(mb, tp);
      o_addMemBlockReference(smb, tp);
    }
    else if((smb->refNum == 1) &&
	    (smb == tp->memBlock))
    {
      O_MDEBUG("moving existing shadow at %p", smb->hdr.key);
      /*
      ** Move the existing shadow.
      */
      (void)VG_(HT_remove)(o_MemBlocks, smb->hdr.key);
      smb->hdr.key = address;
      smb->where = 0; // Dont need this in the shadow.
      VG_(HT_add_node(o_MemBlocks, smb));

      /*
      ** Tweak the existing tracked pointer, leaving the PBit alone.
      */
      tp->block = address;
    }
    else
    {
      /*
      ** A shadow exists and has pointers assigned to it.
      ** We do not allow more than one shadow so deregister and
      ** free this tracked pointer and clear its PBit.
      */
      O_MDEBUG("Prevented second shadow %p (first %p) for %p",
	       address,
	       mb->shadowed,
	       mb->hdr.key);

      o_clearPBit(FROM_TRACKED_KEY(tp->hdr.key));
      o_removeMemBlockReference(NULL, tp);
      VG_(free)(tp);

      o_stats.liveTrackedPointers--;
    }

    O_MDEBUG("shadow creation / reallocation done");
  }
  else if((smb != mb) &&
	  (address == mb->hdr.key))
  {
    /*
    ** Hmmm.
    ** Looks like we are setting the tracked pointer to the block start.
    ** If it was previously pointing at the shadow block, we need to move it
    ** manually.
    */
    if(tp->block == smb->hdr.key)
    {
      O_MDEBUG("moving pointer from shadow to main");

      if(smb->refNum == 1)
      {
	doneShadow = True;

	O_MDEBUG("destroying shadow of %p at %p",
		 mb->hdr.key,
		 smb->hdr.key);
	/*
	** Remove the shadow block and move the pointer.
	*/
	(void)VG_(HT_remove)(o_MemBlocks, smb->hdr.key);
	mb->shadowed = 0;
	VG_(free)(smb->pointers);
	VG_(free)(smb);
	o_stats.liveMemoryBlocks--;

	(void)VG_(HT_remove)(o_TrackedPointers, tp->hdr.key);
	o_addMemBlockReference(mb, tp);
      }
      else
      {
	/*
	** Let the normal code move the pointer.
	*/
      }
    }
  }
  else
  {
    O_MDEBUG("tracked pointer out of range");
  }

  return doneShadow;
}

static void o_killTrackedPointer(Addr addr)
{
  TrackedPointer *tp = VG_(HT_lookup)(o_TrackedPointers, TRACKED_KEY(addr));
  
  /*
  ** We really should have the tracked pointer.
  */
  tl_assert(tp);
  
  /*
  ** Remove the tracked pointer from its memory block, causing
  ** a leak report as required then free it.
  */
  o_clearPBit(addr);
  
  O_MDEBUG("Removing tracked pointer to %p at %p",
	   tp->block, FROM_TRACKED_KEY(tp->hdr.key)); 

  o_removeMemBlockReference(NULL, tp);
  
  VG_(free)(tp);
 
  o_stats.liveTrackedPointers--;
  return;
}

static void o_killRange(Addr start, SizeT length)
{
  /*
  ** We need to check the PBits for the addresses starting at start.
  ** We use the firstPBit / nextPBit functions to get us a list of set
  ** pbits in the specified range.
  */
  PBitContext pb;
  Addr a;

  O_MDEBUG("killing range %p bytes from %p", length, start);

  
  a = o_firstPBit(&pb, start, length);
  while(a)
  {
    o_killTrackedPointer(a);
    a = o_nextPBit(&pb);
  }
  O_MDEBUG("killing range %p bytes from %p done.", length, start);
}

static void o_duplicateTrackedPointers(Addr dst, Addr src, SizeT length)
{
  /*
  ** For each set PBit in the src block, create a new tracked pointer
  ** in the destination block, pointing to the same memory block.
  */
  PBitContext pb;
  Addr address;

  O_MDEBUG("o_duplicateTrackedPointers(%p, %p %d(0x%lx))",
	   dst, src, length, length);

  address = o_firstPBit(&pb, src, length);
    
  while(address)
  {
    /*
    ** Create a tracked pointer at the appropriate place within the new
    ** block of memory.
    */
    TrackedPointer *tp = VG_(HT_lookup)(o_TrackedPointers, TRACKED_KEY(address));
    Int diff           = dst - src;
    TrackedPointer *ntp = VG_(malloc)("om.odTP.1", (sizeof(TrackedPointer)));
    MemBlock       *mb = NULL;
    
    tl_assert(tp);

    o_stats.liveTrackedPointers++;
    o_stats.trackedPointersAllocated++;

    /*
    ** Get the memory block from the tracked pointer at this address.
    */
    mb = tp->memBlock;

    if(!mb)
    {
      O_DEBUG("Oops! Copying pointer at %p to block that leaked(%p)",
	      (void*)address, (void*)(tp->block));
      VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(), VG_(clo_backtrace_size));
      O_DEBUG("");
      
      VG_(tool_panic)("we lost track of a pointer :-(");
    }

    tl_assert(ntp);
    
    VG_(memset)(ntp, 0, sizeof(TrackedPointer));
    ntp->hdr.key = TRACKED_KEY(address + diff);
    o_addMemBlockReference(mb, ntp);
      
    /*
    ** Set the PBit for this tracked pointer.
    */
    o_setPBit(address + diff);
      
    address = o_nextPBit(&pb);
  }

}

static void o_createMemBlock(ThreadId tid, Addr start, SizeT size)
{
  MemBlock *mb = VG_(malloc)("om.ocMB.1", sizeof(MemBlock));
  tl_assert(mb);
  
  o_stats.memoryBlocksAllocated++;
  o_stats.liveMemoryBlocks++;
  
  VG_(memset)(mb, 0, sizeof(MemBlock));
  
  /*
  ** Populate the block. Note that we have no pointers until one is written
  ** into memory.
  */
  mb->hdr.key = start;
  mb->length = size;
  mb->where = VG_(record_ExeContext)(tid, 0/*first_ip_delta*/);
  
  /*
    O_DEBUG("Creating new MemBlock (%p) key = %p, length %d",
    mb, (void *)start, size);
    VG_(pp_ExeContext)(mb->where);
  */
  
  /*
  ** Add this node into the hash table.
  */
  VG_(HT_add_node)(o_MemBlocks, mb);
}

static void o_destroyMemBlock(ThreadId tid, Addr start)
{
  /*
  ** Destroy our memory block.
  */
  MemBlock *mb = VG_(HT_remove)(o_MemBlocks, start);

  /*
  ** The block really should exist, unless this is a double free attempt...
  */
  if(!mb)
  {
    O_DEBUG("Double/Invalid call to free(%p)", (void*)start);
    VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(), VG_(clo_backtrace_size));
    O_DEBUG("");
  }
  else
  {
    if(mb->leaked)
    {
      /*
      ** Seems that the block didnt leak after all.
      ** *sigh*
      ** Why do so many libs access memory in blocks they free()ed?
      */
      if(o_addSuppressionBlock(mb->where, mb->leaked) && !o_showSummaryOnly)
      {
	O_DEBUG("Welcome back (and goodbye) to the supposedly leaked block at %p",
		(void*)start);
      }
      o_stats.memoryBlocksLeaked--;
      o_stats.memoryBlocksLostAndFound++;
    }
    /*
    ** Clean up the block - we pass a pointer pointer so that we can
    ** set it to NULL during the cleanup process.
    */
    o_cleanupMemBlock(&mb);
  }

  return;
}


static void o_setupMaybeLast(Addr a)
{
  int refCount = 0;
  /*
  ** Maybe returning a value - set the maybeLast and funcEnd members
  ** in the memory block this register points to if it is the last
  ** item.
  */
  TrackedPointer *tp = VG_(HT_lookup)(o_TrackedPointers, TRACKED_KEY(a));
  /*
  ** We really should have the tracked pointer.
  */
  tl_assert(tp);
  
  refCount = tp->memBlock->refNum;
  if(tp->memBlock->shadowing)
  {
    refCount += tp->memBlock->shadowing->refNum;
  }
  else if(tp->memBlock->shadowed)
  {
    refCount += tp->memBlock->shadowed->refNum;
  }
  
  if(refCount == 1)
  {
    // Hmmm, last reference. If we haven't already done so,
    // save the context, just in case
    tl_assert(!tp->memBlock->maybeLast ||
              (tp->memBlock->maybeLast == tp));
    if(!tp->memBlock->maybeLast)
    {
      tp->memBlock->maybeLast = tp;
      tp->memBlock->funcEnd = VG_(record_ExeContext)(VG_(get_running_tid)(),
                                                     0/*first_ip_delta*/);
      O_MDEBUG("setting maybeLast to %p in block at %p",
               FROM_TRACKED_KEY(tp->hdr.key), tp->block);
    }
#if defined(O_MASTER_DEBUG)
    else
    {
      O_MDEBUG("leaving maybeLast at %p in block at %p",
               FROM_TRACKED_KEY(tp->hdr.key), tp->block);
    }
#endif
  }
  O_MDEBUG("leaving register %p", OFFSET_FROM_REG(a)); 
}

/*------------------------------------------------------------*/
/*--- Helper functions called by instrumentation            ---*/
/*------------------------------------------------------------*/
#if defined(O_TRACK_LOADS)
static VG_REGPARM(1)
void o_omegaLoadTracker( Addr address )
{
  O_MDEBUG("o_omegaLoadTracker(%p, %p)", address, *((Addr *)address));

  return;
}
#endif

static VG_REGPARM(2)
void o_omegaScratchRemover( Addr start, Addr length )
{
  O_MDEBUG("o_omegaScratchRemover(%p, %p)", start, length);
  o_killRange(start, length);

  return;
}

static VG_REGPARM(1)
void o_endOfInstruction( Addr address )
{
  /*
  ** Any generated leaks should report immediately.
  */
  doLeakNow = True;

  O_MDEBUG("o_endOfInstruction %p doLeakListCount = %d",
           address, doLeakListCount);

  if(doLeakListCount)
  {
    if(doLeakListCount > 1)
    {
      /*
      ** Reverse the list so the reports come out in the correct order.
      */
      MemBlock *front = NULL;
      MemBlock *temp = NULL;

      do
      {
	temp = doLeakList->next;

	if(front)
	{
	  doLeakList->next = front;
	}
	else
	{
	  doLeakList->next = NULL;
	}
	front = doLeakList;

	doLeakList = temp;
      }
      while(doLeakList);

      /*
      ** Now do the leak reports.
      */
      while(front)
      {
	temp = front;
	front = front->next;

	if(temp->doLeak)
	{
	  temp->doLeak = False;
	  o_doLeakReport(temp);
	}
	else
	{
	  O_MDEBUG("block at %p survived!", temp->hdr.key);
	}
      }
    }
    else
    {
      if(doLeakList->doLeak)
      {
	/*
	** The block has leaked. Report it.
	*/
	o_doLeakReport(doLeakList);
      }
      else
      {
	O_MDEBUG("block at %p survived!", doLeakList->hdr.key);
      }

      doLeakList->doLeak = False;
      doLeakList = NULL;
    }
  }

  O_MDEBUG("o_endOfInstruction done");

  o_indirectStack = NULL;
  doLeakListCount = 0;
  doLeakNow = False;
}

static
void o_omegaFunctionReturn( void )
{
  PBitContext pb;
  Addr a = 0;

  /*
  ** Zap scratch registers.
  */

#if defined(VGA_x86)
  a = o_firstPBit(&pb,
		  MAP_TO_REG(VG_(get_running_tid)(), OFFSET_x86_ECX),
		  OFFSET_x86_EDI + 4);
#elif defined(VGA_amd64)
  a = o_firstPBit(&pb,
		  MAP_TO_REG(VG_(get_running_tid)(), OFFSET_amd64_RCX),
		  OFFSET_amd64_R15 + 8);
#endif
  doLeakNow = True;
  while(a)
  {
    if(o_isReturnIgnoreReg(OFFSET_FROM_REG(a)))
    {
      O_MDEBUG("killing register %p", OFFSET_FROM_REG(a)); 
      o_killTrackedPointer(a);
    }
    a = o_nextPBit(&pb);
  }
  doLeakNow = False;

  /*
  ** Now work out if we might be returning a value in the accumulator.
  */
#if defined(VGA_x86)
  a = MAP_TO_REG(VG_(get_running_tid)(), OFFSET_x86_EAX);
#elif defined(VGA_amd64)
  a = MAP_TO_REG(VG_(get_running_tid)(), OFFSET_amd64_RAX);
#endif
  if(o_isPBitSet(a))
    o_setupMaybeLast(a);

#if defined(VGA_amd64)
  // Also need to check for the RDX register as it is a second return reg
  a = MAP_TO_REG(VG_(get_running_tid)(), OFFSET_amd64_RDX);
  if(o_isPBitSet(a))
    o_setupMaybeLast(a);
#endif
  return;
}

static VG_REGPARM(2)
void o_omegaDetector( Addr address, Addr value)
{
  TrackedPointer *tp = NULL;
  MemBlock       *mb = NULL;

  /*
  ** We need to track the registers.
  ** To do this, if the address < 256, change it to our local shadow.
  **
  ** We really want to be able to track the proper shadow but I have no
  ** idea yet how to get the address for it. Once I do, use that in
  ** preference. Note that all we need is a unique memory location for
  ** the register in order to generate a tracked pointer.
  */
  if(address < 0x100)
  {
    O_MDEBUG("o_omegaDetector(%p, %p)", address, value);
    address = MAP_TO_REG(VG_(get_running_tid)(), address);
  }
  else
  {
    /*
    ** Check aligned - if not, align it and retrive the stored value.
    */
    if(address & ~TRACK_MINOR_MASK)
    {
      address &= TRACK_MINOR_MASK;
      value = *((Addr *)address);
    }
    O_MDEBUG("o_omegaDetector(%p, %p)", address, value);
  }

  /*
  ** Done the alignment tweaks so do the more expensive lookups.
  */
  if(o_isPBitSet(address))
  {
    tp = VG_(HT_lookup)(o_TrackedPointers, TRACKED_KEY(address));

    if(tp && (tp->block == value))
    {
      /*
      ** Unlikely but it seems that we are writing the same value back into
      ** the tracked pointer - don't process further for a small gain.
      */
      //O_DEBUG("writing duplicate into tracked pointer.");
      return;
    }
    
    /*
    ** We always auto shadow.
    ** Note that auto shadowing only works if you overwrite a tracked pointer.
    ** Checking for the creation of a new tracked pointer at some internal
    ** address is too much overhead as we would have to scan backwards to find
    ** a memory block then check if the value is within it. For those cases,
    ** we need to get something going with the client request system.
    */
    if(tp && value)
    {
      if(o_setupShadow(tp, value))
      {
	return;
      }
    }

    /*
    ** Remove the tracked pointer and clear the PBit,
    ** if we have one.
    */
    if(tp)
    {
      tl_assert(tp->hdr.key == TRACKED_KEY(address));
      O_MDEBUG("Removing tracked pointer to %p at %p",
	       tp->block, FROM_TRACKED_KEY(tp->hdr.key));
      o_clearPBit(address);
      o_removeMemBlockReference(NULL, tp);
    }
  }

  /*
  ** Get the mem block now - it might not exist if tp was the last
  ** reference to it. It might not exist anyway.
  */
  if(value)
  {
    mb = VG_(HT_lookup)(o_MemBlocks, value);
  }

  /*
  ** If we have a memblock, clean the tracked pointer then add it.
  ** If not, free the tracked pointer.
  */
  if(mb)
  {
    if(!tp)
    {
      /*
      ** No tracked pointer - create one now.
      */
      tp = VG_(malloc)("om.oD.1", sizeof(TrackedPointer));
      tl_assert(tp);
      o_stats.trackedPointersAllocated++;
      o_stats.liveTrackedPointers++;
    }
    VG_(memset)(tp, 0, sizeof(TrackedPointer));
    tp->hdr.key = TRACKED_KEY(address);
    o_addMemBlockReference(mb, tp);
    /*
    ** Set the PBit for this tracked pointer.
    */
    o_setPBit(address);

    O_MDEBUG("Added tracked pointer to %p at %p",
	     tp->block, FROM_TRACKED_KEY(tp->hdr.key));

  }
  else if(tp)
  {
    VG_(free)(tp);
    o_stats.liveTrackedPointers--;
  }
  
  return;
}

/*------------------------------------------------------------*/
/*--- malloc() et al replacement wrappers                  ---*/
/*------------------------------------------------------------*/

static
void* o_newBlock ( ThreadId tid, SizeT size, SizeT align, Bool is_zeroed )
{
  void* p = NULL;

O_TRACE_ON();
#if defined(O_MASTER_DEBUG)
  if(!o_traceStop)
  {
    VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(), 8);O_DEBUG("");
  }
#endif

  O_MDEBUG("newBlock(%d, %d, %d, %d)",
	   tid,
	   size,
	   align,
	   (int)is_zeroed);
  
  /*
  ** Allocate and zero if necessary.
  */
  p = VG_(cli_malloc)( align, size );
  if(!p)
  {
    O_DEBUG("Out of memory!");
    VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(), VG_(clo_backtrace_size));
    O_DEBUG("");

    return NULL;
  }

  if(is_zeroed)
  {
    VG_(memset)(p, 0, size);
  }

  if(!o_onlyMallocLike)
  {
    /*
    ** Create a new MemBlock.
    */
    o_createMemBlock(tid, (Addr)p, size);
  }
  
  O_MDEBUG("o_newBlock returning %p", p);

  return p;
}

static
void o_dieBlock ( ThreadId tid, void* p )
{
  /*
  ** Free off the allocated memory block.
  */
  O_MDEBUG("o_dieBlock(%d, %p)", tid, p);

  /*
  ** Check if we have a potentially valid pointer
  */
  if(!p)
  {
    return;
  }

  /*
  ** If we are doing malloc like block handling, only free off the memory.
  */
  if(!o_onlyMallocLike)
  {
    o_destroyMemBlock(tid, (Addr)p);
  }

  /*
  ** Actually free the heap block.
  */
  VG_(cli_free)( p );

  return;
}

static void* o_malloc ( ThreadId tid, SizeT n )
{
  return o_newBlock( tid, n, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* o__builtin_new ( ThreadId tid, SizeT n )
{
  return o_newBlock( tid, n, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* o__builtin_vec_new ( ThreadId tid, SizeT n )
{
  return o_newBlock( tid, n, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* o_calloc ( ThreadId tid, SizeT m, SizeT size )
{
  return o_newBlock( tid, m*size, VG_(clo_alignment), /*is_zeroed*/True );
}

static void *o_memalign ( ThreadId tid, SizeT align, SizeT n )
{
  return o_newBlock( tid, n, align, False );
}

static void o_free ( ThreadId tid, void* p )
{
  o_dieBlock( tid, p );
}

static void o__builtin_delete ( ThreadId tid, void* p )
{
  o_dieBlock( tid, p );
}

static void o__builtin_vec_delete ( ThreadId tid, void* p )
{
  o_dieBlock( tid, p );
}

static void* o_realloc ( ThreadId tid, void* p_old, SizeT new_size )
{
  MemBlock *mb = NULL;
  void     *p_new = NULL;

  O_MDEBUG("o_realloc p_old %p, new_size %d",
	   p_old, new_size);

  if(!p_old)
  {
    /*
    ** Pointer == NULL so let new block do the work.
    */
    return o_newBlock(tid, new_size, VG_(clo_alignment), /*is_zeroed*/False);
  }

  mb = VG_(HT_lookup)(o_MemBlocks, (Addr)p_old);

  /*
  ** Check that we have this memory block.
  */
  if(!mb)
  {
    /*
    ** Log the bad call but return p_old so the program can continue.
    ** This might not be a good thing but some of the libraries are a
    ** little weird and returning NULL as per the spec blows them up...
    */
    O_DEBUG("Invalid call to realloc(%p)", p_old);
    VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(), VG_(clo_backtrace_size));
    O_DEBUG("");
    
    return p_old;
  }
  
  if(mb->leaked)
  {
    /*
    ** Seems that the block didnt leak after all.
    */
    if(o_addSuppressionBlock(mb->where, mb->leaked) && !o_showSummaryOnly)
    {
      O_DEBUG("Welcome back to the supposedly leaked block at %p",
	      p_old);
    }
    mb->leaked = NULL;
    o_stats.memoryBlocksLeaked--;
    o_stats.memoryBlocksLostAndFound++;
  }

  if(new_size)
  {
    if(new_size > mb->length)
    {
      /*
      ** Make a new block, copy the data into it then free the old block.
      ** We lose all tracked pointers but that is to be expected as this is
      ** a new block at a new address. However, any tracked pointers within
      ** must be preserved.
      */
      
      p_new = o_newBlock(tid, new_size, VG_(clo_alignment), False);
      tl_assert(p_new);
      
      VG_(memcpy)(p_new, p_old, mb->length);
      
      o_duplicateTrackedPointers((Addr)p_new, (Addr)p_old, mb->length);
    }
    else
    {
      /*
      ** Return the existing block.
      */
      return p_old;
    }
  }

  /*
  ** This will remove all of the old tracked pointers within.
  */
  o_dieBlock(tid, p_old);
  
  return p_new;
}

static void o_dieMemStack(Addr start, SizeT length)
{
  /*
  ** Flag that this is a stack unwind.
  */
  o_stackUnwind = True;
  o_killRange(start, length);
  o_stackUnwind = False;
}

static void o_post_clo_init(void)
{
  /*
  ** Allocate the hash tables.
  ** Note that we can improve performance at the cost of memory by initialising
  ** with a larger prime number so more of the key part of the address is
  ** unique. The defaults are probably OK for many programs but we expose them
  ** on the command line to make it easier for users to change them.
  */
  o_PBits = VG_(HT_construct)( "omega pbits" );
  tl_assert(o_PBits);

  o_MemBlocks = VG_(HT_construct)( "omega memblocks" );
  tl_assert(o_MemBlocks);

  o_TrackedPointers = VG_(HT_construct)( "omega tracked ptrs" );
  tl_assert(o_TrackedPointers);

  /*
  ** We need precise instructions so that we can work out the range of the
  ** original machine instruction in terms of grouping together lumps of IR.
  ** We lose out big time on optimisation but we have to take the hit in order
  ** to deal with instructions like pop and xchg.
  */
  VG_(clo_vex_control).iropt_precise_memory_exns = True;

}

static IRSB *
o_instrument(VgCallbackClosure* closure,
			  IRSB* bb_in, 
			  VexGuestLayout* layout, 
			  VexGuestExtents* vge,
			  IRType gWordTy, IRType hWordTy)
{
  IRDirty* di;
  Int      i;
  IRSB*    bb;
  IRType   type;
  Addr     mask;
  IRStmt*  stackReg = NULL;

#if 0 //defined(O_MASTER_DEBUG)

  static int  thisBlock = 0;
  thisBlock++;
  if(thisBlock == 11377)
  {
    O_TRACE_ON();
  }
  else if(thisBlock == 11390)
  {
    VG_(tool_panic)("hit stop block");
  }      
#endif

  if (gWordTy != hWordTy)
  {
    /* We don't currently support this case. */
    VG_(tool_panic)("host/guest word size mismatch");
  }
  
  /*
  ** Set up BB
  */
  bb           = emptyIRSB();
  bb->tyenv    = deepCopyIRTypeEnv(bb_in->tyenv);
  bb->next     = deepCopyIRExpr(bb_in->next);
  bb->jumpkind = bb_in->jumpkind;

#if (VG_WORDSIZE == 4)
  type = Ity_I32;
  mask = ~0x03;
#elif  (VG_WORDSIZE == 8)
  type = Ity_I64;
  mask = ~0x07;
#endif

  for (i = 0; i < bb_in->stmts_used; i++)
  {
    IRStmt* st = bb_in->stmts[i];
    if (!st || st->tag == Ist_NoOp)
    {
      continue;
    }

    di = NULL;
    
    switch (st->tag)
    {
      case Ist_AbiHint:
	/*
	** An area just went undefined. There may be pointers in this
	** scratch area that we should now ignore.
	** Make sure that we do so.
	*/
        if(stackReg)
        {
          addStmtToIRSB( bb, stackReg );
          stackReg = NULL;
        }
	di = unsafeIRDirty_0_N( 2, "o_omegaScratchRemover",
				&o_omegaScratchRemover,
				mkIRExprVec_2(st->Ist.AbiHint.base,
					      mkIRExpr_HWord(st->Ist.AbiHint.len)));
	/*
	** Add in the original instruction second.
	*/
	addStmtToIRSB( bb, IRStmt_Dirty(di) );
	break;

      case Ist_Store:
        if(stackReg)
        {
          addStmtToIRSB( bb, stackReg );
          stackReg = NULL;
        }
	if(typeOfIRExpr(bb->tyenv, st->Ist.Store.addr) == type)
	{
	  /*
	  ** We have an address of native size.
	  */
	  if(typeOfIRExpr(bb->tyenv, st->Ist.Store.data) == type)
	  {
	    /*
	    ** We have data of native size - check if this is a pointer being
	    ** written.
	    */
	    di = unsafeIRDirty_0_N( 2, "o_omegaDetector", &o_omegaDetector,
				    mkIRExprVec_2(st->Ist.Store.addr,
						  st->Ist.Store.data));
	    /*
	    ** Add in the original instruction second.
	    */
	    addStmtToIRSB( bb, IRStmt_Dirty(di) );
	    addStmtToIRSB( bb, st );
	    st = NULL;
	  }
	  else
	  {
	    /*
	    ** There is no way that the data is a pointer but we still have to
	    ** check if a pointer will be overwritten.
	    */
	    di = unsafeIRDirty_0_N( 2, "o_omegaDetector", &o_omegaDetector,
				    mkIRExprVec_2(st->Ist.Store.addr,
						  mkIRExpr_HWord(0)));
	    /*
	    ** Add in the original instruction first.
	    */
	    addStmtToIRSB( bb, st );
	    addStmtToIRSB( bb, IRStmt_Dirty(di) );
	    st = NULL;
	  }
	}
	else
	{
	  O_GDEBUG("o_instrument address type(%p) not a pointer",
		   typeOfIRExpr(bb->tyenv, st->Ist.Store.addr));
	}

	break;

      case Ist_IMark:
	/*
	** Call the end of instruction callback. This is to check what actually
	** leaked as opposed to what appeared to leak in a transient fashion
	** due to instructions getting broken up into more simple IR
	** instructions. Note that stack register updates are moved to
        ** the end of the orginal instruction so that things like 'pop' get
        ** the values into registers BEFORE the stack is invalidated.
	*/
        if(stackReg)
        {
          addStmtToIRSB( bb, stackReg );
          stackReg = NULL;
        }
	di = unsafeIRDirty_0_N( 1, "o_endOfInstruction", &o_endOfInstruction,
				mkIRExprVec_1(mkIRExpr_HWord(st->Ist.IMark.addr)));
	addStmtToIRSB( bb, IRStmt_Dirty(di) );
	addStmtToIRSB( bb, st );
#if defined(VGA_x86)
	/*
	** Make sure the EIP sim cpu register gets updated or our stack
	** traces go a little Pete Tong...
	** If this duplicates, the ir optimisation will knock one of them out.
	*/
	addStmtToIRSB( bb, IRStmt_Put(OFFSET_x86_EIP,
				      mkIRExpr_HWord(st->Ist.IMark.addr)));
#endif
	st = NULL;
	break;

      case Ist_Put:
	/*
	** Track the general purpose registers.
	*/
	switch(st->Ist.Put.offset & mask)
	{
#if defined(VGA_x86)
	  case OFFSET_x86_ESP:
#elif defined(VGA_amd64)
	  case OFFSET_amd64_RSP:
#endif
            /*
            ** Save the stack register update - we will add it at the end of
            ** the instruction.
            */
            stackReg = st;
            st = NULL;
            break;

#if defined(VGA_x86)

	  case OFFSET_x86_EAX:
	  case OFFSET_x86_EBX:
	  case OFFSET_x86_ECX:
	  case OFFSET_x86_EDX:
	  case OFFSET_x86_ESI:
	  case OFFSET_x86_EDI:
	  case OFFSET_x86_EBP:

#if 0 //defined(O_MASTER_DEBUG)
	  case OFFSET_x86_EIP:
#endif

#elif defined(VGA_amd64)

	  case OFFSET_amd64_RAX:
	  case OFFSET_amd64_RBX:
	  case OFFSET_amd64_RCX:
	  case OFFSET_amd64_RDX:
	  case OFFSET_amd64_RSI:
	  case OFFSET_amd64_RDI:
	  case OFFSET_amd64_RBP:
	  case OFFSET_amd64_R8:
	  case OFFSET_amd64_R9:
	  case OFFSET_amd64_R10:
	  case OFFSET_amd64_R11:
	  case OFFSET_amd64_R12:
	  case OFFSET_amd64_R13:
	  case OFFSET_amd64_R14:
	  case OFFSET_amd64_R15:

#if 0 //defined(O_MASTER_DEBUG)
	  case OFFSET_amd64_RIP:
#endif

#elif defined(VGA_ppc32) || defined(VGA_ppc64)
  default:
  VG_(printf)("\nOmega does not currently work on PowerPC/POWER platforms."
              "  Sorry.\n\n");
  VG_(exit)(0);
#else

#error Unknown arch

#endif
	  {
	    if(typeOfIRExpr(bb->tyenv, st->Ist.Put.data) == type)
	    {
	      /*
	      ** This is a put to a register in the simulated processor of data
	      ** that could be a pointer.
	      */
	      di = unsafeIRDirty_0_N( 2, "o_omegaDetector", &o_omegaDetector,
				      mkIRExprVec_2(mkIRExpr_HWord(st->Ist.Put.offset),
						    st->Ist.Put.data));
	    }
	    else
	    {
	      /*
	      ** There is no way that the data is a pointer but we still have
	      ** to check if a pointer in a register will be overwritten.
	      */
	      di = unsafeIRDirty_0_N( 2, "o_omegaDetector", &o_omegaDetector,
				      mkIRExprVec_2(mkIRExpr_HWord(st->Ist.Put.offset),
						    mkIRExpr_HWord(0)));
	    }
	    /*
	    ** Add in the original instruction first.
	    */
	    addStmtToIRSB( bb, st );
	    addStmtToIRSB( bb, IRStmt_Dirty(di) );
	    st = NULL;
	  }
	  break; // Register Cases
	}
	break; // Ist_Put

#if defined(O_TRACK_LOADS)
      case Ist_Tmp:
	/*
	** Debug to see how 'leaked' references survive.
	** (From experience, mostly through illegal reads from
	** free()ed blocks.)
	*/
	if(st->Ist.Tmp.data->tag == Iex_Load)
	{
	  if(typeOfIRExpr(bb->tyenv, st->Ist.Tmp.data->Iex.Load.addr) == type)
	  {
	    di = unsafeIRDirty_0_N( 1, "o_omegaLoadTracker", &o_omegaLoadTracker,
				    mkIRExprVec_1(st->Ist.Tmp.data->Iex.Load.addr));
	    /*
	    ** Add in the original instruction first.
	    */
	    addStmtToIRSB( bb, st );
	    addStmtToIRSB( bb, IRStmt_Dirty(di) );
	    st = NULL;
	  }
	}
	break;
#endif

      default:
	break;
    }

    /*
    ** Add in the original instruction if we havent already done so.
    */
    if(st)
    {
      addStmtToIRSB( bb, st );
    }
  }
  
  if(stackReg)
  {
    addStmtToIRSB( bb, stackReg );
    stackReg = NULL;
  }

  if(bb->jumpkind == Ijk_Ret)
  {
    /*
    ** The client is doing a return. This is the point to invalidate
    ** registers that belong to the callee, possibly generating a
    ** leak report. This is to catch things like foo(malloc(128)).
    */
    
    di = unsafeIRDirty_0_N( 0, "o_omegaFunctionReturn",
			    &o_omegaFunctionReturn,
			    mkIRExprVec_0());
    /*
    ** Add in the new instruction.
    */
    addStmtToIRSB( bb, IRStmt_Dirty(di) );
  }

  return bb;
}

/*------------------------------------------------------------*/
/*--- Client Request Handling                              ---*/
/*------------------------------------------------------------*/
static Bool o_handle_client_request ( ThreadId tid, UWord* arg, UWord* ret )
{
  if (!VG_IS_TOOL_USERREQ('O','M',arg[0]) &&
      VG_USERREQ__MALLOCLIKE_BLOCK != arg[0] &&
      VG_USERREQ__FREELIKE_BLOCK   != arg[0])
    return False;

  switch (arg[0])
  {
    case VG_USERREQ__ENTERING_MAIN:
    {
      /*
      ** Allow leak reports whilst inside main().
      */
      o_inhibitLeakDetect = False;
    }
    break;

    case VG_USERREQ__LEAVING_MAIN:
    {
      /*
      ** Stop any more leak reports - they won't be helpfull.
      */
      o_inhibitLeakDetect = True;

O_TRACE_OFF();

    }
    break;

    case VG_USERREQ__MALLOCLIKE_BLOCK:
    {
      if(o_onlyMallocLike)
      {
	/*
	** Either we use malloc like block or we don't.
	** Trying to auto track and do malloc like block handling together
	** is asking for trouble.
	*/
	Addr p     = (Addr)arg[1];
	SizeT size =       arg[2];
       
	o_createMemBlock(tid, p, size);
      }
    }
    break;
     
    case VG_USERREQ__FREELIKE_BLOCK:
    {
      if(o_onlyMallocLike)
      {
	/*
	** Either we use malloc like block or we don't.
	** Trying to auto track and do malloc like block handling together
	** is asking for trouble.
	*/
	Addr p = (Addr)arg[1];
	 
	o_destroyMemBlock(tid, p);
      }
    }
    break;
  }
   
  return True;
}

/*------------------------------------------------------------*/
/*--- Circular Reference Detection                         ---*/
/*------------------------------------------------------------*/
/*
** Check for circular references. This is where a memory block holds a
** reference to another memory block and vice versa but there are no
** references that are external. Like this:

  typedef struct
  {
    void *linkedBlock;
    char padding[120];
  } block;

  block *p1 = NULL;
  block *p2 = NULL;

  p1 = (block *)malloc(sizeof(block));
  p2 = (block *)malloc(sizeof(block));

  p1->linkedBlock = p2;
  p2->linkedBlock = p1;

** As you can see, the blocks wont be seen to leak because they have a live
** reference but the reality is that without an external reference, these
** blocks are lost to the system.
**
** To perform this test, we go through the following stages:
** 
**  1) Generate a binary tree of the memory covered by the allocated blocks
**  2) Check every tracked pointer of every allocated block and mark the
**     block if any of them fall outside of an allocated block.
**  3) For each block with an external pointer, recursivly walk through the
**     internal pointers to other blocks, marking the blocks as also having
**     an external pointer.
**  4) Report any blocks without external references.
**
*/

typedef struct _TreeNode{
  Addr              start;
  Addr              end;
  MemBlock         *block;
  struct _TreeNode *left;
  struct _TreeNode *right;
} TreeNode;

static TreeNode       *o_treeRoot = NULL;
static MemBlock      **o_memblockList = NULL;
static UInt            o_memblockListCount = 0;
static BlockRecordList o_circularRecords = {NULL, NULL};

static
TreeNode *o_findTreeNode(Addr addr, TreeNode *start, TreeNode ***parent)
{
  /*
  ** Find the treenode that this address falls within and return it.
  ** Return NULL if no matching node is found and return the parent if it is
  ** requested.
  */

  /*
  ** If the treeRoot is NULL, we won't be finding anything.
  */
  if(!o_treeRoot)
  {
    if(parent)
    {
      *parent = &o_treeRoot;
    }

    return NULL;
  }

  /*
  ** The start should be a valid node.
  */
  tl_assert(start);

  if((addr >= start->start) &&
     (addr <= start->end))
  {
    /*
    ** Found it
    */
    return start;
  }

  if(addr < start->start)
  {
    /*
    ** Less than - go left if we can, return NULL if we can't.
    */
    if(start->left)
    {
      return o_findTreeNode(addr, start->left, parent);
    }
    else
    {
      if(parent)
      {
	*parent = &start->left;
      }

      return NULL;
    }
  }
  else
  {
    /*
    ** Greater than - go right if we can, return NULL if we can't.
    */
    if(start->right)
    {
      return o_findTreeNode(addr, start->right, parent);
    }
    else
    {
      if(parent)
      {
	*parent = &start->right;
      }

      return NULL;
    }
  }

  VG_(tool_panic)("fell out of the binary tree");
}

static UInt o_buildMemblockTree(void)
{
  /*
  ** Build a binary tree of the addresses covered by the memory blocks.
  ** We dont do anything to balance things so this could decompose to a
  ** linear structure. Thankfully, we are not in a time critical section.
  */
  UInt indx;

  o_memblockList = (MemBlock **)VG_(HT_to_array)(o_MemBlocks,
						 &o_memblockListCount);

  for(indx = 0; indx < o_memblockListCount; indx++)
  {
    TreeNode **parent = NULL;
    TreeNode *tn = NULL;
    MemBlock *mb = o_memblockList[indx];

    /*
    ** Only process main blocks that havent leaked.
    */
    if(!mb->shadowing && !mb->leaked)
    {
      if(o_findTreeNode(mb->hdr.key, o_treeRoot, &parent))
      {
	VG_(tool_panic)("Failed to grow the binary tree.");
      }

      /*
      ** We should have a pointer to the parent
      */
      tl_assert(parent);

      /*
      ** Create and populate the new node
      */
      tn = VG_(malloc)("om.obMbT.1", sizeof(TreeNode));
      VG_(memset)(tn, 0, sizeof(TreeNode));
      
      tn->start = mb->hdr.key;
      tn->end = tn->start + mb->length;
      tn->block = mb;
      
      /*
      ** Add this node into the parent node
      */
      *parent = tn;
    }
  }

  return o_memblockListCount;
}

static void o_checkExternalPointers(void)
{
  UInt indx;

  for(indx = 0; indx < o_memblockListCount; indx++)
  {
    MemBlock *mb = o_memblockList[indx];

    /*
    ** Only check blocks that haven't leaked.
    ** We process through shadow blocks because we want the back references
    ** as they still point within the shadowed block.
    */
    if(!mb->leaked)
    {
      UInt pointerIndex;

      for(pointerIndex = 0; pointerIndex < mb->refNum; pointerIndex++)
      {
	if(!o_findTreeNode(FROM_TRACKED_KEY(mb->pointers[pointerIndex]->hdr.key),
			   o_treeRoot, NULL))
	{
	  /*
	  ** External reference. Mark the block and stop checking.
	  */
	  mb->external = 1;
	  break;
	}
      }
    }
  }  
}

static void o_rippleExternelPointers(MemBlock *mb)
{
  UInt indx;

  if(!mb)
  {
    /*
    ** Iterate through the memory block list marking external blocks
    ** so that we dont process the same blocks twice.
    */
    for(indx = 0; indx < o_memblockListCount; indx++)
    {
      if(o_memblockList[indx]->external > 0)
      {
	o_memblockList[indx]->external = -1;
	o_rippleExternelPointers(o_memblockList[indx]);
      }
    }
  }
  else
  {
    /*
    ** We are recursing.
    ** Follow any tracked pointers within our block, marking the target
    ** blocks as external and recursing on those blocks.
    */
    PBitContext pb;
    Addr a;
    TreeNode *tn = NULL;

    a = o_firstPBit(&pb, mb->hdr.key, mb->length);
    while(a)
    {
      tn = o_findTreeNode(a, o_treeRoot, NULL);

      /*
      ** We really should have a node
      */
      tl_assert(tn);

      /*
      ** If we havent already done so, mark the block as external and
      ** processed then recurse on it.
      */
      if(tn->block->external >= 0)
      {
	tn->block->external = -1;
	o_rippleExternelPointers(tn->block);
      }

      /*
      ** Get the next tracked pointer within this block.
      */
      a = o_nextPBit(&pb);
    }
  }
}

static int o_reportCircularBlocks(void)
{
  int count = 0;
  BlockRecord *block = NULL;
  int indx;

  /*
  ** Iterate through the memory block list reporting any blocks not marked
  ** as external.
  ** We aggregate the list of blocks as many could come from the same context.
  */
  for(indx = 0; indx < o_memblockListCount; indx++)
  {
    MemBlock * mb = o_memblockList[indx];
    if(!mb->shadowing && !mb->leaked && mb->external == 0)
    {
      block = o_findBlockRecord(&o_circularRecords, mb->where, NULL);

      if(block)
      {
	/*
	** Just increment the counts.
	*/
	block->bytes += mb->length;
	block->count++;
      }
      else
      {
	/*
	** Create a new block and add it to the circular records list.
	*/
	BlockRecord *item = VG_(malloc)("om.orCB.1", sizeof(BlockRecord));
	tl_assert(item);
	
	item->count = 1;
	item->bytes = mb->length;
	item->next = item->prev = NULL;
	item->allocated = mb->where;
	item->leaked = NULL;

	o_addBlockRecord(&o_circularRecords, item);
      }
    }
  }

  /*
  ** Now report the blocks.
  */
  block = o_circularRecords.start;
  while(block)
  {
    if(!count)
    {
      VG_(message)(Vg_UserMsg, "The following blocks only have circular references from other blocks");
    }
    count++;

    VG_(message)(Vg_UserMsg, " Circular loss record %d", count);
    VG_(message)(Vg_UserMsg, "   Leaked %d (0x%x) bytes in %ld block%sallocated",
		 block->bytes,
		 block->bytes,
		 block->count,
		 (block->count == 1) ? " " : "s ");
    VG_(pp_ExeContext)(block->allocated);
    VG_(message)(Vg_UserMsg,"");

    /*
    ** Get the next block, if any.
    */
    block = block->next;
  }

  return count;
}

static int o_checkCircular(void)
{
  int count = 0;

  /*
  ** If there is nothing in the tree, there is nothing to check.
  */
  if(o_buildMemblockTree())
  {
    o_checkExternalPointers();
    o_rippleExternelPointers(NULL);
    count = o_reportCircularBlocks();
  }

  return count;
}

static void o_fini(Int exitcode)
{
  /*
  ** Iterate through the leaked block record list,
  ** printing out the stats as we go.
  */
  UInt         count  = 1;
  BlockRecord *record = o_leakRecords.start;

  VG_(message)(Vg_UserMsg,"");
  VG_(message)(Vg_UserMsg,"");
  VG_(message)(Vg_UserMsg,"Omega Leak Summary");
  VG_(message)(Vg_UserMsg,"==================");

  while(record)
  {
    VG_(message)(Vg_UserMsg,
		 "Loss Record %d: Leaked %d (0x%x) bytes in %ld block%s",
		 count, record->bytes, record->bytes, record->count,
		 (record->count > 1) ? "s" : "");
    VG_(pp_ExeContext)(record->leaked);
    VG_(message)(Vg_UserMsg, " Block%s allocated",
		 (record->count > 1) ? "s" : "");
    VG_(pp_ExeContext)(record->allocated);
    VG_(message)(Vg_UserMsg,"");

    count++;
    record = record->next;
  }

  if(o_showCircular)
  {
    /*
    ** Now check for circular references.
    */
    count += o_checkCircular();
  }

  if(count == 1)
  {
    /*
    ** Nothing leaked - assure the user.
    */
    VG_(message)(Vg_UserMsg,"No leaks to report.");
    VG_(message)(Vg_UserMsg,"");
  }

  /*
  ** Remove the leaked blocks from the live blocks count - they wont be
  ** coming back now...
  */
  o_stats.liveMemoryBlocks -= o_stats.memoryBlocksLeaked;

  if(o_showInternStats)
  {
    VG_(printf)("\n\n\n"
		"Omega internal statistics summary:\n"
		"  Tracked Pointers still live:    %ld\n"
		"  Tracked Pointers Allocated:     %ld\n"
		"  Memory Blocks still live:       %ld\n"
		"  Memory Blocks Allocated:        %ld\n"
		"  Shadow Memory Blocks Allocated: %ld\n"
		"  Memory Blocks Leaked:           %ld\n"
		"  Memory Blocks Lost and Found:   %ld\n"
		"  pbitNodes:                      %ld\n\n",
		o_stats.liveTrackedPointers,
		o_stats.trackedPointersAllocated,
		o_stats.liveMemoryBlocks,
		o_stats.memoryBlocksAllocated,
		o_stats.shadowMemoryBlocksAllocated,
		o_stats.memoryBlocksLeaked,
		o_stats.memoryBlocksLostAndFound,
		o_stats.pbitNodes);
  }
}

static Bool o_process_cmd_line_option(Char *arg)
{
  /*
  ** Setup our processing state based upon what the user would like us to do.
  */
  Int pbithash = 0;
  Int mbhash = 0;
  Int tphash = 0;

  /*
  ** Expose the hash sizes for simple performance tweaking.
  */
  VG_NUM_CLO(arg, "--pbithashsize", pbithash);
  VG_NUM_CLO(arg, "--mbhashsize", mbhash);
  VG_NUM_CLO(arg, "--tphashsize", tphash);

  /*
  ** Only tweak upwards for now.
  */
  if(pbithash > o_pbitNodeHashSize)
    o_pbitNodeHashSize = pbithash;

  if(mbhash > o_memblockHashSize)
    o_memblockHashSize = mbhash;

  if(tphash > o_trackedPointerHashSize)
    o_trackedPointerHashSize = tphash;

  /*
  ** Check the flags.
  */
  if(VG_CLO_STREQ(arg, "--only-malloclike"))
    o_onlyMallocLike = True;
  else if(VG_CLO_STREQ(arg, "--show-indirect"))
    o_showIndirect = True;
  else if(VG_CLO_STREQ(arg, "--show-circular"))
    o_showCircular = True;
  else if(VG_CLO_STREQ(arg, "--show-hanging"))
    o_showHanging = True;
  else if(VG_CLO_STREQ(arg, "--show-intern-stats"))
    o_showInternStats = True;
  else if(VG_CLO_STREQ(arg, "--instant-reports"))
    o_showSummaryOnly = False;
  else if(VG_CLO_STREQ(arg, "--poison"))
    o_poison = True;
  else
    return VG_(replacement_malloc_process_cmd_line_option)(arg);

  return True;
}

static void o_print_usage(void)
{
  /*
  ** Tell the average user what we support.
  */
  VG_(printf)("");
  VG_(printf)(
"    --only-malloclike        only track blocks passed through the\n"
"                             MALLOCLIKE_BLOCK user request.\n"
"    --show-indirect          show indirect leaks from leaked blocks.\n"
"    --show-circular          show blocks that just have circular references.\n"
"    --instant-reports        show leaks as they happen, not just a summary.\n"
"    --show-hanging           show hanging pointers to the block being\n"
"                             deallocated.\n"
  );

}

static void o_print_debug_usage(void)
{
  /*
  ** Tell the inquisitive user what else we support.
  */
  VG_(printf)("");
  VG_(printf)(
"    --show-intern-stats      show some internal statistics from the run.\n"
"\n"
"         IMPORTANT! These next settings must be PRIME NUMBERS\n"
"\n"
"    --pbithashsize=<number>  number of pbit nodes to allocate [%d]\n"
"    --mbhashsize=<number>    number of mem block nodes to allocate [%d]\n"
"    --tphashsize=<number>    number of tracked pointer nodes to allocate [%d]\n",
  o_pbitNodeHashSize,
  o_memblockHashSize,
  o_trackedPointerHashSize
  );
}

static void o_memRemapSupport(Addr src, Addr dst, SizeT length)
{
  /*
  ** The track_copy_mem_remap callback has the src and dst the opposite
  ** way around to our duplicate tracked pointers function so this tiny
  ** wrapper twizzles them around.
  */
  o_duplicateTrackedPointers(dst, src, length);
}

static void o_pre_clo_init(void)
{
  // Details
  VG_(details_name)            ("exp-omega");
  VG_(details_version)         ("RC1");
  VG_(details_description)     ("an instant memory leak detector");
  VG_(details_copyright_author)("Copyright (C) 2006-2008, and GNU GPL'd, "
                                "by Bryan Meredith.");
  VG_(details_bug_reports_to)  ("richard.coe@med.ge.com");
  
  // Basic functions
  VG_(basic_tool_funcs)        (o_post_clo_init,
				o_instrument,
				o_fini);
  // Needs
  VG_(needs_malloc_replacement) (o_malloc,
				 o__builtin_new,
				 o__builtin_vec_new,
				 o_memalign,
				 o_calloc,
				 o_free,
				 o__builtin_delete,
				 o__builtin_vec_delete,
				 o_realloc,
				 0 );
  // Want stack unwinds
  VG_(track_die_mem_stack)      (o_dieMemStack);
  // Need command line input
  VG_(needs_command_line_options) (o_process_cmd_line_option,
				   o_print_usage,
				   o_print_debug_usage);
  // Support MALLOCLIKE and FREELIKE
  VG_(needs_client_requests)     (o_handle_client_request);

  // Wholesale destruction of memory ranges
  VG_(track_copy_mem_remap)      (o_memRemapSupport );
  VG_(track_die_mem_stack_signal)(o_killRange); 
  VG_(track_die_mem_brk)         (o_killRange);
  VG_(track_die_mem_munmap)      (o_killRange); 

}

VG_DETERMINE_INTERFACE_VERSION(o_pre_clo_init);

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
