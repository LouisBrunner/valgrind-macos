
#include "scalar.h"

int main(void)
{
   // uninitialised, but we know px[0] is 0x0
   long* px  = malloc(sizeof(long));
   long  x0  = px[0];

   // All __NR_xxx numbers are taken from x86

   // 250--299
   
   // __NR_fadvise64 250
 //GO(__NR_fadvise64, ".s .m");
 //SY(__NR_fadvise64);

   // 251 --> sys_ni_syscall()
   GO(251, "0e");
   SY(251);

   // __NR_exit_group 252
   // See below

   // __NR_lookup_dcookie 253 --> sys_lookup_dcookie()
   GO(__NR_lookup_dcookie, "4s 1m");
   SY(__NR_lookup_dcookie, x0, x0, x0, x0+1);

   // __NR_epoll_create 254
 //GO(__NR_epoll_create, ".s .m");
 //SY(__NR_epoll_create);

   // __NR_epoll_ctl 255
 //GO(__NR_epoll_ctl, ".s .m");
 //SY(__NR_epoll_ctl);

   // __NR_epoll_wait 256
 //GO(__NR_epoll_wait, ".s .m");
 //SY(__NR_epoll_wait);

   // __NR_remap_file_pages 257
 //GO(__NR_remap_file_pages, ".s .m");
 //SY(__NR_remap_file_pages);

   // __NR_set_tid_address 258
 //GO(__NR_set_tid_address, ".s .m");
 //SY(__NR_set_tid_address);

   // __NR_timer_create 259
 //GO(__NR_timer_create, ".s .m");
 //SY(__NR_timer_create);

   // __NR_timer_settime (__NR_timer_create+1)
 //GO(__NR_timer_settime, ".s .m");
 //SY(__NR_timer_settime);

   // __NR_timer_gettime (__NR_timer_create+2)
 //GO(__NR_timer_gettime, ".s .m");
 //SY(__NR_timer_gettime);

   // __NR_timer_getoverrun (__NR_timer_create+3)
 //GO(__NR_timer_getoverrun, ".s .m");
 //SY(__NR_timer_getoverrun);

   // __NR_timer_delete (__NR_timer_create+4)
 //GO(__NR_timer_delete, ".s .m");
 //SY(__NR_timer_delete);

   // __NR_clock_settime (__NR_timer_create+5)
 //GO(__NR_clock_settime, ".s .m");
 //SY(__NR_clock_settime);

   // __NR_clock_gettime (__NR_timer_create+6)
 //GO(__NR_clock_gettime, ".s .m");
 //SY(__NR_clock_gettime);

   // __NR_clock_getres (__NR_timer_create+7)
 //GO(__NR_clock_getres, ".s .m");
 //SY(__NR_clock_getres);

   // __NR_clock_nanosleep (__NR_timer_create+8)
 //GO(__NR_clock_nanosleep, ".s .m");
 //SY(__NR_clock_nanosleep);

   // __NR_statfs64 268
 //GO(__NR_statfs64, ".s .m");
 //SY(__NR_statfs64);

   // __NR_fstatfs64 269
 //GO(__NR_fstatfs64, ".s .m");
 //SY(__NR_fstatfs64);

   // __NR_tgkill 270
 //GO(__NR_tgkill, ".s .m");
 //SY(__NR_tgkill);

   // __NR_utimes 271
 //GO(__NR_utimes, ".s .m");
 //SY(__NR_utimes);

   // __NR_fadvise64_64 272
 //GO(__NR_fadvise64_64, ".s .m");
 //SY(__NR_fadvise64_64);

   // __NR_vserver 273 --> sys_ni_syscall()
 //GO(__NR_vserver, "0e");
 //SY(__NR_vserver);

   // __NR_mbind 274
 //GO(__NR_mbind, ".s .m");
 //SY(__NR_mbind);

   // __NR_get_mempolicy 275
 //GO(__NR_get_mempolicy, ".s .m");
 //SY(__NR_get_mempolicy);

   // __NR_set_mempolicy 276
 //GO(__NR_set_mempolicy, ".s .m");
 //SY(__NR_set_mempolicy);

   // __NR_mq_open  277
 //GO(__NR_mq_open, ".s .m");
 //SY(__NR_mq_open);

   // __NR_mq_unlink (__NR_mq_open+1)
 //GO(__NR_mq_unlink, ".s .m");
 //SY(__NR_mq_unlink);

   // __NR_mq_timedsend (__NR_mq_open+2)
 //GO(__NR_mq_timedsend, ".s .m");
 //SY(__NR_mq_timedsend);

   // __NR_mq_timedreceive (__NR_mq_open+3)
 //GO(__NR_mq_timedreceive, ".s .m");
 //SY(__NR_mq_timedreceive);

   // __NR_mq_notify (__NR_mq_open+4)
 //GO(__NR_mq_notify, ".s .m");
 //SY(__NR_mq_notify);

   // __NR_mq_getsetattr (__NR_mq_open+5)
 //GO(__NR_mq_getsetattr, ".s .m");
 //SY(__NR_mq_getsetattr);
   
   // __NR_sys_kexec_load 283 --> sys_ni_syscall()
 //GO(__NR_sys_kexec_load, "0e");
 //SY(__NR_sys_kexec_load);

   GO(9999, "1e");
   SY(9999);

   // __NR_exit 252 --> sys_exit_group()
   GO(__NR_exit_group, "1s 0m");
   SY(__NR_exit_group, x0);

   assert(0);
}

