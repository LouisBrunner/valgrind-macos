/* -*- mode: C; c-basic-offset: 3; -*- */
#ifndef __PUB_CORE_DRD_H
#define __PUB_CORE_DRD_H


#include "drd_basics.h"
#include "pub_tool_basics.h"


extern Bool DRD_(g_any_address_traced);


void DRD_(suppression_set_trace)(const Bool trace_suppression);
void DRD_(suppression_init)(void);
void DRD_(start_suppression)(const Addr a1, const Addr a2,
                             const char* const reason);
void DRD_(finish_suppression)(const Addr a1, const Addr a2);
Bool DRD_(is_suppressed)(const Addr a1, const Addr a2);
Bool DRD_(is_any_suppressed)(const Addr a1, const Addr a2);
void DRD_(mark_hbvar)(const Addr a1);
Bool DRD_(range_contains_suppression_or_hbvar)(const Addr a1, const Addr a2);
void DRD_(start_tracing_address_range)(const Addr a1, const Addr a2);
void DRD_(stop_tracing_address_range)(const Addr a1, const Addr a2);
Bool DRD_(is_any_traced)(const Addr a1, const Addr a2);
void DRD_(suppression_stop_using_mem)(const Addr a1, const Addr a2);


static __inline__ Bool DRD_(any_address_is_traced)(void)
{
   return DRD_(g_any_address_traced);
}


#endif // __PUB_CORE_DRD_H
