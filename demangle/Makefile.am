INCLUDES += -I$(top_srcdir)

noinst_HEADERS = \
	ansidecl.h     \
        dyn-string.h   \
        demangle.h     \
        safe-ctype.h 

noinst_LIBRARIES = libdemangle.a

libdemangle_a_SOURCES = \
	cp-demangle.c cplus-dem.c dyn-string.c safe-ctype.c

# some files don't like my config.h, so just pretend it does not exist...

cp-demangle.o:
	$(COMPILE) -Wno-unused -Wno-shadow -c $< -UHAVE_CONFIG_H

dyn-string.o:
	$(COMPILE) -c $< -UHAVE_CONFIG_H

cplus-dem.o:
	$(COMPILE) -Wno-unused -c $<
