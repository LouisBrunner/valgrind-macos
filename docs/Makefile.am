
##-------------------------------------------------------------
## HACK: stuff to avoid making the print docs if installing the
## tools to do so is impractical / too difficult
##-------------------------------------------------------------

# Comment out the next line to skip building print docs.  The default
# is not to skip building print docs.  Note, after changing it
# you need to re-run autogen.sh and configure to make it take effect.
BUILD_ALL_DOCS=yes

# Whether to run xmlto pdf --with-fop
# This is de fault, set to empty to use the default fo to pdf
# processor that xmlto uses (which might be pdfxmltex, which is
# known to be broken).
WITH_FOP=--with-fop

##-------------------------------------------------------------
## END OF HACK
##-------------------------------------------------------------

EXTRA_DIST = \
	README \
	images/home.png \
	images/next.png \
	images/prev.png \
	images/up.png \
	images/kcachegrind_xtree.png \
	images/dh-tree.png \
	internals/3_0_BUGSTATUS.txt \
	internals/3_1_BUGSTATUS.txt \
	internals/3_2_BUGSTATUS.txt \
	internals/3_3_BUGSTATUS.txt \
	internals/3_4_BUGSTATUS.txt \
	internals/3_5_BUGSTATUS.txt \
	internals/3_7_BUGSTATUS.txt \
	internals/3_8_BUGSTATUS.txt \
	internals/3_9_BUGSTATUS.txt \
	internals/3_10_BUGSTATUS.txt \
	internals/3_11_BUGSTATUS.txt \
	internals/3_12_BUGSTATUS.txt \
	internals/3_13_BUGSTATUS.txt \
	internals/3_14_BUGSTATUS.txt \
	internals/3_15_BUGSTATUS.txt \
	internals/MERGE_3_10_1.txt \
	internals/arm_thumb_notes_gdbserver.txt \
	internals/avx-notes.txt \
	internals/BIG_APP_NOTES.txt \
	internals/Darwin-debug.txt \
	internals/Darwin-notes.txt \
	internals/SPEC-notes.txt \
	internals/directory-structure.txt \
	internals/howto_BUILD_KDE42.txt \
	internals/howto_oprofile.txt \
	internals/m_replacemalloc.txt \
	internals/m_syswrap.txt \
	internals/module-structure.txt \
	internals/multiple-architectures.txt \
	internals/notes.txt \
	internals/performance.txt \
	internals/porting-HOWTO.txt \
	internals/mpi2entries.txt \
	internals/porting-to-ARM.txt \
	internals/qemu-aarch64-linux-HOWTO.txt \
	internals/qemu-mips64-linux-HOWTO.txt \
	internals/register-uses.txt \
	internals/release-HOWTO.txt \
	internals/segments-seginfos.txt \
	internals/t-chaining-notes.txt \
	internals/threads-syscalls-signals.txt \
	internals/tm-mutexstates.dot \
	internals/tm-threadstates.dot \
	internals/tracking-fn-entry-exit.txt \
	internals/why-no-libc.txt \
	internals/xml-output.txt \
	internals/xml-output-protocol4.txt \
	lib/line-wrap.xsl \
	lib/vg_basic.css \
	lib/vg-fo.xsl \
	lib/vg-faq2txt.xsl \
	lib/vg-html-chunk.xsl \
	lib/vg-html-website.xsl \
	lib/vg-html-common.xsl \
	xml/FAQ.xml \
	xml/dist-docs.xml \
	xml/index.xml \
	xml/licenses.xml \
	xml/manpages-index.xml \
	xml/manual.xml \
	xml/manual-intro.xml \
	xml/manual-core.xml \
	xml/manual-core-adv.xml \
	xml/manual-writing-tools.xml \
	xml/design-impl.xml \
	xml/quick-start-guide.xml \
	xml/tech-docs.xml \
	xml/valgrind-manpage.xml \
	xml/vg-entities.xml \
	xml/xml_help.txt


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

if VGCONF_OS_IS_FREEBSD
XML_CATALOG_FILES = /usr/local/share/xml/catalog
else
XML_CATALOG_FILES = /etc/xml/catalog
endif

# file to log print output to
LOGFILE = print.log

# validation stuff
XMLLINT       = xmllint
XMLLINT_FLAGS = --noblanks --noout --xinclude --postvalid --noent \
		--path $(top_builddir)/docs/xml

# stylesheet processor
XSLTPROC       = xsltproc
XSLTPROC_FLAGS = --nonet --xinclude --path $(top_builddir)/docs/xml

# stylesheets
XSL_HTML_CHUNK_STYLE  = $(mylibdir)/vg-html-chunk.xsl
XSL_FO_STYLE          = $(mylibdir)/vg-fo.xsl
XSL_TEXT_STYLE        = $(mylibdir)/vg-faq2txt.xsl
XSL_WEBSITE_STYLE     = $(mylibdir)/vg-html-website.xsl
XSL_MAN_STYLES = \
    /usr/local/share/xsl/docbook/manpages/docbook.xsl \
    /usr/share/sgml/docbook/xsl-stylesheets/manpages/docbook.xsl \
    /usr/share/xml/docbook/stylesheet/nwalsh/current/manpages/docbook.xsl \
    /usr/share/xml/docbook/stylesheet/nwalsh/manpages/docbook.xsl

# Input files for valid-manual and valid-manpages
MANUAL_XML_FILES = $(myxmldir)/index.xml \
		     $(top_builddir)/docs/xml/vg-entities.xml \
		     $(myxmldir)/quick-start-guide.xml \
		     $(myxmldir)/manual.xml \
		       $(myxmldir)/manual-intro.xml \
		       $(myxmldir)/manual-core.xml \
		       $(myxmldir)/manual-core-adv.xml \
		     $(top_srcdir)/*/docs/*-manual.xml \
		     $(myxmldir)/quick-start-guide.xml \
		     $(myxmldir)/FAQ.xml \
		     $(myxmldir)/tech-docs.xml \
		       $(myxmldir)/design-impl.xml \
		       $(myxmldir)/manual-writing-tools.xml \
		       $(top_srcdir)/callgrind/docs/cl-format.xml \
		     $(myxmldir)/dist-docs.xml \
		       $(top_srcdir)/AUTHORS \
		       $(top_srcdir)/NEWS \
		       $(top_srcdir)/NEWS.old \
		       $(top_srcdir)/README \
		       $(top_srcdir)/README_MISSING_SYSCALL_OR_IOCTL \
		       $(top_srcdir)/README_DEVELOPERS \
		       $(top_srcdir)/README_PACKAGERS \
		       $(top_srcdir)/README.s390 \
		       $(top_srcdir)/README.android \
		       $(top_srcdir)/README.android_emulator \
		       $(top_srcdir)/README.mips \
		       $(top_srcdir)/README.solaris \
		     $(myxmldir)/licenses.xml \
		       $(top_srcdir)/COPYING \
		       $(top_srcdir)/COPYING.DOCS

MANPAGES_XML_FILES = $(myxmldir)/manpages-index.xml \
		       $(myxmldir)/valgrind-manpage.xml \
			 $(myxmldir)/manual-core.xml \
		       $(top_srcdir)/*/docs/*-manual.xml

all-docs: FAQ.txt man-pages html-docs print-docs

valid-manual: $(MANUAL_XML_FILES)
	if type $(XMLLINT) 2>/dev/null; then \
	  $(XMLLINT) $(XMLLINT_FLAGS) $(myxmldir)/index.xml && \
	  touch $@; \
	fi

valid-manpages: $(MANPAGES_XML_FILES)
	if type $(XMLLINT) 2>/dev/null; then \
	  $(XMLLINT) $(XMLLINT_FLAGS) $(myxmldir)/manpages-index.xml && \
	  touch $@; \
	fi

check-local: valid-manual valid-manpages

# The text version of the FAQ.
FAQ.txt:
	@echo "Generating the text version of the FAQ ..."
	export XML_CATALOG_FILES=$(XML_CATALOG_FILES) && \
	$(XSLTPROC) $(XSLTPROC_FLAGS) -o ./FAQ.txt $(XSL_TEXT_STYLE) $(myxmldir)/FAQ.xml

# the valgrind manpages
man-pages:
	set -e; \
	for x in $(XSL_MAN_STYLES) ; do \
	    if test -f $$x; then \
		echo "Using manpage stylesheet: $$x"; \
		export XML_CATALOG_FILES=$(XML_CATALOG_FILES) && \
		$(XSLTPROC) $(XSLTPROC_FLAGS) -o ./ $$x $(myxmldir)/manpages-index.xml; \
		exit 0; \
	    fi; \
	done; \
	echo ""; \
	echo "Error: I can't find the XSL_MAN_STYLE file";     \
	echo "Please check where it lives on your system, and"      \
	     "amend the line 'XSL_MAN_STYLES = ' in this Makefile."; \
	exit 1;

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
	cp $(myimgdir)/*.png $(myprintdir)/images && \
	xmlto -v $(WITH_FOP) --stringparam=keep.relative.image.uris="1" \
	      -x $(XSL_FO_STYLE) \
	      --searchpath $(top_builddir)/docs/xml:print \
	      pdf $(myxmldir)/index.xml &> $(LOGFILE) && \
	echo "Generating PS file: $(myprintdir)/index.ps ..." && \
	pdftops index.pdf && \
	mv index.pdf index.ps $(myprintdir) && \
	rm $(LOGFILE)

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
	set -e; \
	if test -r html ; then \
		mkdir -p $(DESTDIR)$(datadir)/doc/valgrind; \
		cp -r html $(DESTDIR)$(datadir)/doc/valgrind; \
	fi
	for f in *.1 ; do \
		if test -r $$f ; then \
			mkdir -p $(DESTDIR)$(mandir)/man1; \
			cp $$f $(DESTDIR)$(mandir)/man1; \
		fi \
	done
 ifeq ($(BUILD_ALL_DOCS),yes)
	set -e; \
	if test -r index.pdf ; then \
		mkdir -p $(DESTDIR)$(datadir)/doc/valgrind; \
		cp index.pdf $(DESTDIR)$(datadir)/doc/valgrind/valgrind_manual.pdf; \
	fi
	set -e; \
	if test -r index.ps ; then \
		mkdir -p $(DESTDIR)$(datadir)/doc/valgrind; \
		cp index.ps $(DESTDIR)$(datadir)/doc/valgrind/valgrind_manual.ps; \
	fi
 endif

# This is done at 'make dist' time.  It builds the html docs, print
# docs and man pages and copies them into the docs/ directory in the
# tarball.
 ifeq ($(BUILD_ALL_DOCS),yes)
dist-hook: FAQ.txt html-docs man-pages print-docs
	cp -r html $(distdir)
	cp FAQ.txt $(distdir)/..
	cp *.1 $(distdir)
	cp print/index.pdf $(distdir)
	cp print/index.ps $(distdir)
 else
dist-hook: FAQ.txt html-docs man-pages
	cp -r html $(distdir)
	cp FAQ.txt $(distdir)/..
	cp *.1 $(distdir)
 endif

distclean-local:
	rm -rf html print
	rm -f FAQ.txt
	rm -f $(top_builddir)/FAQ.txt $(top_builddir)/docs/*.1

clean-local:
	rm -f valid-manual valid-manpages


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
	cp $(myimgdir)/*.png $(vgdir)/print/images/
	xmlto -v $(WITH_FOP) --stringparam=keep.relative.image.uris="1" \
	      -x $(XSL_FO_STYLE) \
	      --searchpath $(top_builddir)/docs/xml:print \
	      pdf $(myxmldir)/index.xml &> $(LOGFILE)
	mv index.pdf $(vgdir)/print/manual.pdf
	@echo ""
	@echo "Generating valgrind_manual.ps.bz2 ..."
	(cd $(vgdir)/print/ && \
		( pdftops manual.pdf ) )
	mv $(vgdir)/print/manual.pdf $(downloadsdir)/valgrind_manual.pdf
	mv $(vgdir)/print/manual.ps $(downloadsdir)/valgrind_manual.ps
	bzip2 $(downloadsdir)/valgrind_manual.ps
	/bin/rm -fr $(vgdir) $(LOGFILE)
