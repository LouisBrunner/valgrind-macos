SUBDIRS = xml lib images

EXTRA_DIST = README porting-HOWTO

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
	export XML_CATALOG_FILES=$(XML_CATALOG_FILES)
	mkdir -p $(htmldir)
	/bin/rm -fr $(htmldir)/
	mkdir -p $(htmldir)/
	mkdir -p $(htmldir)/images
	cp $(libdir)/vg_basic.css $(htmldir)/
	cp $(imgdir)/*.png $(htmldir)/images
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o $(htmldir)/ $(XSL_HTML_CHUNK_STYLE) $(xmldir)/index.xml

# pdf and postscript
print-docs:
	@echo "Generating PDF file: $(printdir)/index.pdf (please be patient)...";
	export XML_CATALOG_FILES=$(XML_CATALOG_FILES);
	mkdir -p $(printdir);
	mkdir -p $(printdir)/images;
	cp $(imgdir)/massif-graph-sm.png $(printdir)/images;
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o $(printdir)/index.fo $(XSL_FO_STYLE) $(xmldir)/index.xml;
	(cd $(printdir); \
	 pdfxmltex index.fo &> $(LOGFILE); \
	 pdfxmltex index.fo &> $(LOGFILE); \
	 pdfxmltex index.fo &> $(LOGFILE); \
	 echo "Generating PS file: $(printdir)/index.ps ..."; \
	 pdftops index.pdf; \
	 rm *.log *.aux *.fo *.out)

# If the docs have been built, install them.  But don't worry if they have 
# not -- developers do 'make install' not from a 'make dist'-ified distro all
# the time.
install-data-hook:
	if test -r html ; then \
		mkdir -p $(datadir)/doc/valgrind/; \
		cp -r html $(datadir)/doc/valgrind/; \
	fi

dist-hook: html-docs
	cp -r html $(distdir)	

distclean-local:
	rm -rf html print
