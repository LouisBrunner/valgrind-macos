// Small test program to demonstrate Valgrind bug.
// https://bugs.kde.org/show_bug.cgi?id=254164


#include <stdio.h>
#include <sys/resource.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/sysctl.h>
#include <mach/task.h>
#include <mach/mach_init.h>

void getres(task_t task, unsigned int *rss, unsigned int *vs)
{
    struct task_basic_info t_info;
    mach_msg_type_number_t t_info_count = TASK_BASIC_INFO_COUNT;
    
    task_info(task, TASK_BASIC_INFO, (task_info_t)&t_info, &t_info_count);
    *rss = t_info.resident_size;
    *vs = t_info.virtual_size;
}

/** It appears task_set_info() is a deprecated interface on modern Darwin
  * Per comments in osfmk/kern/task.c:
  *
  *   This routine was added, pretty much exclusively, for registering the
  *   RPC glue vector for in-kernel short circuited tasks. Rather than
  *   removing it completely, I have only disabled that feature (which was
  *   the only feature at the time).
  */
/**
void setres(task_t task)
{
    struct task_trace_memory_info t_info;
    mach_msg_type_number_t t_info_count = TASK_TRACE_MEMORY_INFO_COUNT;
 
    t_info.user_memory_address = NULL;
    t_info.buffer_size = 0;
    t_info.mailbox_array_size = 0;
    
    task_set_info(task, TASK_TRACE_MEMORY_INFO, (task_info_t)&t_info, &t_info_count);
}
 */

int main(void)
{
    unsigned int rss, vs;
    task_t task = MACH_PORT_NULL;
    
    if (task_for_pid(current_task(), getpid(), &task) != KERN_SUCCESS)
        abort();
    
    getres(task, &rss, &vs);
    //setres(task);
    
    return 0;
}

