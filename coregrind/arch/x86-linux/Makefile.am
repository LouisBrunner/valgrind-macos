
valdir = $(libdir)/valgrind
val_PROGRAMS = libpthread.so

libpthread_so_SOURCES = \
        vg_libpthread.c \
        vg_libpthread_unimp.c \
        vg_syscall.S
libpthread_so_DEPENDENCIES = $(srcdir)/vg_libpthread.vs
libpthread_so_LDFLAGS = -Werror -fno-omit-frame-pointer -UVG_LIBDIR \
        -shared -fpic \
        -Wl,-version-script $(srcdir)/vg_libpthread.vs \
        -Wl,-z,nodelete \
        -Wl,--soname=libpthread.so.0
