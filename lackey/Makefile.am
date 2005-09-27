include $(top_srcdir)/Makefile.tool.am

val_PROGRAMS = lackey

lackey_SOURCES      = lk_main.c
lackey_DEPENDENCIES = $(COREGRIND_LIBS)
lackey_LDADD        = $(TOOL_LINKADD)
lackey_LDFLAGS      = $(TOOL_LINKFLAGS)
