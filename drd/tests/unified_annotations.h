#ifndef _UNIFIED_ANNOTATIONS_H_
#define _UNIFIED_ANNOTATIONS_H_


#include "../../drd/drd.h"


/*
 * Define annotation macros such that these can be intercepted by DRD, Helgrind
 * and ThreadSanitizer. See also
 * http://code.google.com/p/data-race-test/source/browse/trunk/dynamic_annotations/dynamic_annotations.h
 */
#define U_ANNOTATE_NEW_MEMORY(addr, size) ANNOTATE_NEW_MEMORY(addr, size)
#define U_ANNOTATE_HAPPENS_BEFORE(addr) U_AnnotateHappensBefore(addr)
#define U_ANNOTATE_HAPPENS_AFTER(addr) U_AnnotateHappensAfter(addr)
#define U_ANNOTATE_HAPPENS_DONE(addr)		\
  do {						\
    ANNOTATE_HAPPENS_DONE(addr);		\
  } while(0)


#ifdef __cplusplus
extern "C" {
#endif
#if 0
}
#endif


void  __attribute__((weak,noinline))
AnnotateCondVarSignal(const char *file, int line, const volatile void *cv)
{
  asm("");
}

void  __attribute__((weak,noinline))
AnnotateCondVarWait(const char *file, int line, const volatile void *cv,
		    const volatile void *lock)
{
  asm("");
}

static __inline__ void U_AnnotateHappensBefore(void* addr)
{
  ANNOTATE_HAPPENS_BEFORE(addr);
  AnnotateCondVarSignal(__FILE__, __LINE__, addr);
}

static __inline__ void U_AnnotateHappensAfter(void *addr)
{
  ANNOTATE_HAPPENS_AFTER(addr);
  AnnotateCondVarWait(__FILE__, __LINE__, addr, NULL);
}


#if 0
{
#endif
#ifdef __cplusplus
}
#endif


#endif /* _UNIFIED_ANNOTATIONS_H_ */

/*
 * Local variables:
 * c-basic-offset: 2
 * End:
 */
