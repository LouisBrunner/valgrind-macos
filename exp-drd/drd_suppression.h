#ifndef __PUB_CORE_DRD_H
#define __PUB_CORE_DRD_H


#include "pub_tool_basics.h"


extern Bool g_any_address_traced;


void suppression_set_trace(const Bool trace_suppression);
void drd_suppression_init(void);
void drd_start_suppression(const Addr a1, const Addr a2,
                           const char* const reason);
void drd_finish_suppression(const Addr a1, const Addr a2);
Bool drd_is_suppressed(const Addr a1, const Addr a2);
Bool drd_is_any_suppressed(const Addr a1, const Addr a2);
void drd_start_tracing_address_range(const Addr a1, const Addr a2);
void drd_stop_tracing_address_range(const Addr a1, const Addr a2);
Bool drd_is_any_traced(const Addr a1, const Addr a2);
void drd_suppression_stop_using_mem(const Addr a1, const Addr a2);


static __inline__ Bool drd_any_address_is_traced(void)
{
  return g_any_address_traced;
}


#endif // __PUB_CORE_DRD_H
