
GLIBC_2.0 {
};

GLIBC_2.1 {
} GLIBC_2.0;

GLIBC_2.2 {
} GLIBC_2.1;

GLIBC_2.2.3 {
   __pthread_clock_gettime;
   __pthread_clock_settime;
} GLIBC_2.2;

GLIBC_PRIVATE {
   __pthread_clock_gettime;
   __pthread_clock_settime;
};
