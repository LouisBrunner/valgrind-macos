
#include "scalar.h"

int main(void)
{
   // uninitialised, but we know px[0] is 0x0
   long* px  = malloc(sizeof(long));
   long  x0  = px[0];

   // All __NR_xxx numbers are taken from x86

   // 200--249
   
   // __NR_getgid32 200 --> sys_getgid()
   GO(__NR_getgid32, "0e");
   SY(__NR_getgid32);

   // __NR_geteuid32 201 --> sys_geteuid()
   GO(__NR_geteuid32, "0e");
   SY(__NR_geteuid32);

   // __NR_getegid32 202 --> sys_getegid()
   GO(__NR_getegid32, "0e");
   SY(__NR_getegid32);

   // __NR_setreuid32 203
   GO(__NR_setreuid32, "2s 0m");
   SY(__NR_setreuid32, x0, x0);

   // __NR_setregid32 204
   GO(__NR_setregid32, "2s 0m");
   SY(__NR_setregid32, x0, x0);

   // __NR_getgroups32 205 --> sys_getgroups()
   GO(__NR_getgroups32, "2s 1m");
   SY(__NR_getgroups32, x0+1, x0+1);

   // __NR_setgroups32 206 --> sys_setgroups()
   GO(__NR_setgroups32, "2s 1m");
   SY(__NR_setgroups32, x0+1, x0+1);

   // __NR_fchown32 207 --> sys_fchown()
   GO(__NR_fchown32, "3s 0m");
   SY(__NR_fchown32, x0, x0, x0);

   // __NR_setresuid32 208 --> sys_setresuid()
   GO(__NR_setresuid32, "3s 0m");
   SY(__NR_setresuid32, x0, x0, x0);

   // __NR_getresuid32 209 --> sys_getresuid()
   GO(__NR_getresuid32, "3s 3m");
   SY(__NR_getresuid32, x0, x0, x0);

   // __NR_setresgid32 210 --> sys_setresgid()
   GO(__NR_setresgid32, "3s 0m");
   SY(__NR_setresgid32, x0, x0, x0);

   // __NR_getresgid32 211 --> sys_getresgid()
   GO(__NR_getresgid32, "3s 3m");
   SY(__NR_getresgid32, x0, x0, x0);

   // __NR_chown32 212 --> sys_chown()
   GO(__NR_chown32, "3s 1m");
   SY(__NR_chown32, x0, x0, x0);

   // __NR_setuid32 213 --> sys_setuid()
   GO(__NR_setuid32, "1s 0m");
   SY(__NR_setuid32, x0);

   // __NR_setgid32 214
   GO(__NR_setgid32, "1s 0m");
   SY(__NR_setgid32, x0);

   // __NR_setfsuid32 215 --> sys_setfsuid()
   GO(__NR_setfsuid32, "1s 0m");
   SY(__NR_setfsuid32, x0);

   // __NR_setfsgid32 216 --> sys_setfsgid()
   GO(__NR_setfsgid32, "1s 0m");
   SY(__NR_setfsgid32, x0);

   // __NR_pivot_root 217
 //GO(__NR_pivot_root, ".s .m");
 //SY(__NR_pivot_root);

   // __NR_mincore 218
 //GO(__NR_mincore, ".s .m");
 //SY(__NR_mincore);

   // __NR_madvise 219
 //GO(__NR_madvise, ".s .m");
 //SY(__NR_madvise);

   // __NR_getdents64 220
 //GO(__NR_getdents64, ".s .m");
 //SY(__NR_getdents64);

   // __NR_fcntl64 221
 //GO(__NR_fcntl64, ".s .m");
 //SY(__NR_fcntl64);

   // 222 --> sys_ni_syscall()
   GO(222, "0e");
   SY(222);

   // 223 --> sys_ni_syscall()
   GO(223, "0e");
   SY(223);

   // __NR_gettid 224
 //GO(__NR_gettid, ".s .m");
 //SY(__NR_gettid);

   // __NR_readahead 225
 //GO(__NR_readahead, ".s .m");
 //SY(__NR_readahead);

   // __NR_setxattr 226 --> sys_xattr()
   GO(__NR_setxattr, "5s 3m");
   SY(__NR_setxattr, x0, x0, x0, x0+1, x0);

   // __NR_lsetxattr 227 --> sys_lsetxattr()
   GO(__NR_lsetxattr, "5s 3m");
   SY(__NR_lsetxattr, x0, x0, x0, x0+1, x0);

   // __NR_fsetxattr 228 --> sys_fsetxattr()
   GO(__NR_fsetxattr, "5s 2m");
   SY(__NR_fsetxattr, x0, x0, x0, x0+1, x0);

   // __NR_getxattr 229 --> sys_getxattr()
   GO(__NR_getxattr, "4s 3m");
   SY(__NR_getxattr, x0, x0, x0, x0+1);

   // __NR_lgetxattr 230 --> sys_lgetxattr()
   GO(__NR_lgetxattr, "4s 3m");
   SY(__NR_lgetxattr, x0, x0, x0, x0+1);

   // __NR_fgetxattr 231 --> sys_fgetxattr()
   GO(__NR_fgetxattr, "4s 2m");
   SY(__NR_fgetxattr, x0, x0, x0, x0+1);

   // __NR_listxattr 232 --> sys_listxattr()
   GO(__NR_listxattr, "3s 2m");
   SY(__NR_listxattr, x0, x0, x0+1);

   // __NR_llistxattr 233 --> sys_llistxattr()
   GO(__NR_llistxattr, "3s 2m");
   SY(__NR_llistxattr, x0, x0, x0+1);

   // __NR_flistxattr 234 --> sys_flistxattr()
   GO(__NR_flistxattr, "3s 1m");
   SY(__NR_flistxattr, x0, x0, x0+1);

   // __NR_removexattr 235 --> sys_removexattr()
   GO(__NR_removexattr, "2s 2m");
   SY(__NR_removexattr, x0, x0);

   // __NR_lremovexattr 236 --> sys_lremovexattr()
   GO(__NR_lremovexattr, "2s 2m");
   SY(__NR_lremovexattr, x0, x0);

   // __NR_fremovexattr 237 --> sys_fremovexattr()
   GO(__NR_fremovexattr, "2s 1m");
   SY(__NR_fremovexattr, x0, x0);

   // __NR_tkill 238
 //GO(__NR_tkill, ".s .m");
 //SY(__NR_tkill);

   // __NR_sendfile64 239
 //GO(__NR_sendfile64, ".s .m");
 //SY(__NR_sendfile64);

   // __NR_futex 240
 //GO(__NR_futex, ".s .m");
 //SY(__NR_futex);

   // __NR_sched_setaffinity 241
 //GO(__NR_sched_setaffinity, ".s .m");
 //SY(__NR_sched_setaffinity);

   // __NR_sched_getaffinity 242
 //GO(__NR_sched_getaffinity, ".s .m");
 //SY(__NR_sched_getaffinity);

   // __NR_set_thread_area 243
 //GO(__NR_set_thread_area, ".s .m");
 //SY(__NR_set_thread_area);

   // __NR_get_thread_area 244
 //GO(__NR_get_thread_area, ".s .m");
 //SY(__NR_get_thread_area);

   // __NR_io_setup 245
 //GO(__NR_io_setup, ".s .m");
 //SY(__NR_io_setup);

   // __NR_io_destroy 246
 //GO(__NR_io_destroy, ".s .m");
 //SY(__NR_io_destroy);

   // __NR_io_getevents 247
 //GO(__NR_io_getevents, ".s .m");
 //SY(__NR_io_getevents);

   // __NR_io_submit 248
 //GO(__NR_io_submit, ".s .m");
 //SY(__NR_io_submit);

   // __NR_io_cancel 249
 //GO(__NR_io_cancel, ".s .m");
 //SY(__NR_io_cancel);

   return 0;
}

