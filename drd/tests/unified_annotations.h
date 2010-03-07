#ifndef _UNIFIED_ANNOTATIONS_H_
#define _UNIFIED_ANNOTATIONS_H_


#include "../../drd/drd.h"


/*
 * Redefine the happens before/after/done annotation macros such that these
 * can be intercepted by DRD, Helgrind and ThreadSanitizer. See also
 * http://code.google.com/p/data-race-test/source/browse/trunk/dynamic_annotations/dynamic_annotations.h
 */
#undef ANNOTATE_HAPPENS_BEFORE
#define ANNOTATE_HAPPENS_BEFORE(addr)			\
  do {							\
    DRDCL_(annotate_happens_before)(addr);		\
    AnnotateCondVarSignal(__FILE__, __LINE__, addr);	\
  } while(0)
#undef ANNOTATE_HAPPENS_AFTER
#define ANNOTATE_HAPPENS_AFTER(addr)				\
  do {								\
    DRDCL_(annotate_happens_after)(addr);			\
    AnnotateCondVarWait(__FILE__, __LINE__, addr, NULL);	\
  } while(0)
#undef ANNOTATE_HAPPENS_DONE
#define ANNOTATE_HAPPENS_DONE(addr)		\
  do {						\
    DRDCL_(annotate_happens_done)(addr);	\
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
