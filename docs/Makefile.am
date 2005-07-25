
##-------------------------------------------------------------
## HACK: stuff to avoid making the print docs if installing the
## tools to do so is impractical / too difficult
##-------------------------------------------------------------

# Comment out the next line to skip building print docs.  The default
# is not to skip building print docs.  Note, after changing it
# you of course need to re-run configure to make it take effect.
BUILD_ALL_DOCS=yes

##-------------------------------------------------------------
## END OF HACK
##-------------------------------------------------------------


SUBDIRS = xml lib images internals

EXTRA_DIST = README

dist_man_MANS = valgrind.1


##-------------------------------------------------------------------
## Below here is more ordinary make stuff...
##-------------------------------------------------------------------
docdir   = ./
xmldir   = $(docdir)xml
imgdir   = $(docdir)images
libdir   = $(docdir)lib
htmldir  = $(docdir)html
printdir = $(docdir)print

XML_CATALOG_FILES = /etc/xml/catalog

# file to log print output to
LOGFILE = print.log

# validation stuff
XMLLINT       = xmllint
LINT_FLAGS    = --noout --xinclude --noblanks --postvalid
VALID_FLAGS   = --dtdvalid http://www.oasis-open.org/docbook/xml/4.2/docbookx.dtd 
XMLLINT_FLAGS = $(LINT_FLAGS) $(VALID_FLAGS)

# stylesheet processor
XSLTPROC       = xsltproc
XSLTPROC_FLAGS = --nonet --xinclude 

# stylesheets
XSL_HTML_CHUNK_STYLE  = $(libdir)/vg-html-chunk.xsl
XSL_HTML_SINGLE_STYLE = $(libdir)/vg-html-single.xsl
XSL_FO_STYLE          = $(libdir)/vg-fo.xsl

all-docs: html-docs print-docs

valid:
	$(XMLLINT) $(XMLLINT_FLAGS) $(xmldir)/index.xml

# chunked html
html-docs:
	@echo "Generating html files..."
	export XML_CATALOG_FILES=$(XML_CATALOG_FILES) && \
	mkdir -p $(htmldir) && \
	/bin/rm -fr $(htmldir)/ && \
	mkdir -p $(htmldir)/ && \
	mkdir -p $(htmldir)/images && \
	cp $(libdir)/vg_basic.css $(htmldir)/ && \
	cp $(imgdir)/*.png $(htmldir)/images && \
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o $(htmldir)/ $(XSL_HTML_CHUNK_STYLE) $(xmldir)/index.xml

# pdf and postscript
print-docs:
	@echo "Generating PDF file: $(printdir)/index.pdf (please be patient)...";
	export XML_CATALOG_FILES=$(XML_CATALOG_FILES) && \
	mkdir -p $(printdir) && \
	mkdir -p $(printdir)/images && \
	cp $(imgdir)/massif-graph-sm.png $(printdir)/images && \
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o $(printdir)/index.fo $(XSL_FO_STYLE) $(xmldir)/index.xml && \
	(cd $(printdir) && \
         ( pdfxmltex index.fo && \
	   pdfxmltex index.fo && \
	   pdfxmltex index.fo ) &> $(LOGFILE) && \
	 echo "Generating PS file: $(printdir)/index.ps ..." && \
	 pdftops index.pdf && \
	 rm -f *.log *.aux *.fo *.out)

# If the docs have been built, install them.  But don't worry if they have 
# not -- developers do 'make install' not from a 'make dist'-ified distro all
# the time.
#
# Note: this is done at 'make install' time.
# Note 2: the ifeq/else/endif have to be indented one space 
# because otherwise it seems that automake thinks it should
# be the one to handle the else/endif parts, not GNU make
# as we intend.
install-data-hook:
	if test -r html ; then \
		mkdir -p $(DESTDIR)$(datadir)/doc/valgrind/; \
		cp -r html $(DESTDIR)$(datadir)/doc/valgrind/; \
	fi
 ifeq ($(BUILD_ALL_DOCS),yes)
	if test -r index.pdf ; then \
		mkdir -p $(DESTDIR)$(datadir)/doc/valgrind/; \
		cp index.pdf $(DESTDIR)$(datadir)/doc/valgrind/; \
	fi
	if test -r index.ps ; then \
		mkdir -p $(DESTDIR)$(datadir)/doc/valgrind/; \
		cp index.ps $(DESTDIR)$(datadir)/doc/valgrind/; \
	fi
 endif


# This is done at 'make dist' time.  It builds the html and print docs
# and copies them into the docs/ directory in the tarball.
 ifeq ($(BUILD_ALL_DOCS),yes)
dist-hook: html-docs print-docs
	cp -r html $(distdir)
	cp print/index.pdf $(distdir)
	cp print/index.ps $(distdir)
 else
dist-hook: html-docs
	cp -r html $(distdir)
 endif

distclean-local:
	rm -rf html print
