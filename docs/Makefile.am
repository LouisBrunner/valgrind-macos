
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


##-------------------------------------------------------------------
## Below here is more ordinary make stuff...
##-------------------------------------------------------------------
myxmldir   = $(top_srcdir)/docs/xml
myimgdir   = $(top_srcdir)/docs/images
mylibdir   = $(top_srcdir)/docs/lib

myhtmldir  = $(top_builddir)/docs/html
myprintdir = $(top_builddir)/docs/print

websitedir   = $(top_builddir)/docs/website
valkyriedir  = $(top_builddir)/docs/vg-html
downloadsdir = $(top_builddir)/docs/downloads
vgdir        = $(top_builddir)/docs/valgrind

XML_CATALOG_FILES = /etc/xml/catalog

# file to log print output to
LOGFILE = print.log

# validation stuff
XMLLINT       = xmllint
LINT_FLAGS    = --noblanks --noout --xinclude --postvalid --noent
# validate with docbook 4.3 'cos it supports xml:base natively
VALID_FLAGS   = --dtdvalid http://www.oasis-open.org/docbook/xml/4.3/docbookx.dtd 
XMLLINT_FLAGS = $(LINT_FLAGS) $(VALID_FLAGS)

# stylesheet processor
XSLTPROC       = xsltproc
XSLTPROC_FLAGS = --nonet --xinclude

# stylesheets
XSL_HTML_CHUNK_STYLE  = $(mylibdir)/vg-html-chunk.xsl
XSL_FO_STYLE          = $(mylibdir)/vg-fo.xsl
XSL_TEXT_STYLE        = $(mylibdir)/vg-faq2txt.xsl
XSL_WEBSITE_STYLE     = $(mylibdir)/vg-html-website.xsl
XSL_MAN_STYLE = /usr/share/xml/docbook/stylesheet/nwalsh/current/manpages/docbook.xsl

all-docs: FAQ.txt man-page html-docs print-docs

valid:
	$(XMLLINT) $(XMLLINT_FLAGS) $(myxmldir)/index.xml

# The text version of the FAQ.
FAQ.txt:
	@echo "Generating the text version of the FAQ ..."
	export XML_CATALOG_FILES=$(XML_CATALOG_FILES) && \
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o ./FAQ.txt $(XSL_TEXT_STYLE) $(myxmldir)/FAQ.xml

# the valgrind manpage
man-page:
	if test ! -f $(XSL_MAN_STYLE); then \
	    echo "Error: I can't find the file '$(XSL_MAN_STYLE)'";     \
	    echo "Please check where it lives on your system, and"      \
	         "amend the line 'XSL_MAN_STYLE = ' in this Makefile."; \
		  exit 1; \
	fi;
	export XML_CATALOG_FILES=$(XML_CATALOG_FILES) && \
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o ./ $(XSL_MAN_STYLE) $(myxmldir)/valgrind-manpage.xml

# chunked html, on a chapter-by-chapter basis
html-docs:
	@echo "Generating html files..."
	export XML_CATALOG_FILES=$(XML_CATALOG_FILES) && \
	mkdir -p $(myhtmldir) && \
	/bin/rm -fr $(myhtmldir)/ && \
	mkdir -p $(myhtmldir)/ && \
	mkdir -p $(myhtmldir)/images && \
	cp $(mylibdir)/vg_basic.css $(myhtmldir)/ && \
	cp $(myimgdir)/*.png $(myhtmldir)/images && \
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o $(myhtmldir)/ $(XSL_HTML_CHUNK_STYLE) $(myxmldir)/index.xml

# pdf and postscript
print-docs:
	@echo "Generating PDF file: $(myprintdir)/index.pdf (please be patient)...";
	export XML_CATALOG_FILES=$(XML_CATALOG_FILES) && \
	mkdir -p $(myprintdir) && \
	mkdir -p $(myprintdir)/images && \
	cp $(myimgdir)/massif-graph-sm.png $(myprintdir)/images && \
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o $(myprintdir)/index.fo $(XSL_FO_STYLE) $(myxmldir)/index.xml && \
	(cd $(myprintdir) && \
         ( pdfxmltex index.fo && \
	   pdfxmltex index.fo && \
	   pdfxmltex index.fo ) &> $(LOGFILE) < /dev/null && \
	 echo "Generating PS file: $(myprintdir)/index.ps ..." && \
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
		mkdir -p $(DESTDIR)$(datadir)/doc/valgrind; \
		cp -r html $(DESTDIR)$(datadir)/doc/valgrind; \
	fi
 ifeq ($(BUILD_ALL_DOCS),yes)
	if test -r index.pdf ; then \
		mkdir -p $(DESTDIR)$(datadir)/doc/valgrind; \
		cp index.pdf $(DESTDIR)$(datadir)/doc/valgrind/valgrind_manual.pdf; \
	fi
	if test -r index.ps ; then \
		mkdir -p $(DESTDIR)$(datadir)/doc/valgrind; \
		cp index.ps $(DESTDIR)$(datadir)/doc/valgrind/valgrind_manual.ps; \
	fi
	if test -r valgrind.1 ; then \
		mkdir -p $(DESTDIR)$(mandir)/man1; \
		cp valgrind.1 $(DESTDIR)$(mandir)/man1/valgrind.1; \
	fi
 endif

# This is done at 'make dist' time.  It builds the html docs, print
# docs and man page and copies them into the docs/ directory in the
# tarball.
 ifeq ($(BUILD_ALL_DOCS),yes)
dist-hook: FAQ.txt html-docs print-docs man-page
	cp -r html $(distdir)
	cp FAQ.txt $(distdir)/..
	cp valgrind.1 $(distdir)
	cp print/index.pdf $(distdir)
	cp print/index.ps $(distdir)
 else
dist-hook: FAQ.txt html-docs man-page
	cp FAQ.txt $(distdir)/..
	cp -r html $(distdir)
 endif

distclean-local:
	rm -rf html print
	rm -f $(top_builddir)/FAQ.txt



# -----------------------------------------------------------------------
# Note: the rest of this file is nothing to do with the normal build
# tree.  The stuff below contains special targets for web-site
# artefacts.

# chunked html with no html/body tags, css, or top nav, to fit into the website
website-docs:
	@echo "Generating website html files ..."
	export XML_CATALOG_FILES=$(XML_CATALOG_FILES)
	/bin/rm -fr $(websitedir)
	mkdir -p $(websitedir)
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o $(websitedir)/ $(XSL_WEBSITE_STYLE) $(myxmldir)/index.xml

# valkyrie carries around her own copy of the valgrind manual
valkyrie-docs:
	@echo "Generating a set of valgrind docs for valkyrie..."
	export XML_CATALOG_FILES=$(XML_CATALOG_FILES)
	/bin/rm -fr $(valkyriedir)
	mkdir -p $(valkyriedir)
	mkdir -p $(valkyriedir)/images
	cp $(mylibdir)/vg_basic.css $(valkyriedir)/
	cp $(myimgdir)/*.png $(valkyriedir)/images/
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o $(valkyriedir)/ $(XSL_HTML_CHUNK_STYLE) $(myxmldir)/index.xml

# html.tar.bz2, .pdf, .ps.bz2 files for downloading from the website
download-docs:
	export XML_CATALOG_FILES=$(XML_CATALOG_FILES)
	/bin/rm -fr $(downloadsdir)
	mkdir -p $(downloadsdir)
	@echo ""
	@echo "Generating valgrind_manual.html.tar.bz2 ..."
	export XML_CATALOG_FILES=$(XML_CATALOG_FILES)
	/bin/rm -fr $(vgdir)
	mkdir -p $(vgdir)
	mkdir -p $(vgdir)/html
	mkdir -p $(vgdir)/html/images
	cp $(mylibdir)/vg_basic.css $(vgdir)/html/
	cp $(myimgdir)/*.png $(vgdir)/html/images/
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o $(vgdir)/html/ $(XSL_HTML_CHUNK_STYLE) $(myxmldir)/index.xml
	(cd $(top_builddir)/docs/ && \
		(tar cfj $(downloadsdir)/valgrind_manual.html.tar.bz2 ./valgrind/html/ ) )
	/bin/rm -fr $(vgdir)/html/
	@echo ""
	@echo "Generating valgrind_manual.pdf ..."
	mkdir -p $(vgdir)/print
	mkdir -p $(vgdir)/print/images
	cp $(myimgdir)/massif-graph-sm.png $(vgdir)/print/images/
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o $(vgdir)/print/manual.fo $(XSL_FO_STYLE) $(myxmldir)/index.xml
	(cd $(vgdir)/print/ && \
		( pdfxmltex manual.fo && \
		  pdfxmltex manual.fo && \
		  pdfxmltex manual.fo ) &> $(LOGFILE) < /dev/null )
	@echo ""
	@echo "Generating valgrind_manual.ps.bz2 ..."
	(cd $(vgdir)/print/ && \
		( pdftops manual.pdf ) )
	mv $(vgdir)/print/manual.pdf $(downloadsdir)/valgrind_manual.pdf
	mv $(vgdir)/print/manual.ps $(downloadsdir)/valgrind_manual.ps
	bzip2 $(downloadsdir)/valgrind_manual.ps
	/bin/rm -fr $(vgdir)
