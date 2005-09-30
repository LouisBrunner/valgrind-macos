README.txt file for the FAQ.xml to FAQ.txt transformer.
=======================================================

In valgrind-3.1.SVN, file docs/README contains, under the heading
"TODO CRUCIAL", the item "Need to generate text FAQ from the
valgrind/docs/xml/FAQ.xml (done at 'make dist' time along with the
HTML docs using the 'dist-hook'), and remove the old text FAQ which is
in valgrind/."  This is an attempt to resolve this item using xsltproc
and a number of xml style sheets. It is a hack in the sense that it
does not support all of docbook, but only the subset currently used by
file docs/xml/FAQ.xml.

The transformation is done in two stages:

1. Inline elements (literal, computeroutput and ulink) are replaced
   with the corresponding text by stylesheet untag-inline.xslt. This
   stylesheet uses copy.xslt to handle the non-inline elements.

2. The actual formatting is done by stylesheet docbook2text.xslt. It
   uses stylesheets str.dup.xslt, str.find-last.xslt,
   text.justify.xslt and text.wrap.xslt to handle the formatting of
   the text into a column with the approproate width and indentation.

Stylesheets untag-inline.xslt and docbook2text.xslt are original
work. Stylesheets copy.xslt, str.dup.xslt, str.find-last.xslt,
text.justify.xslt and text.wrap.xslt are copied with some adaptations
from the examples supplied with the XSLT Cookbook by Sal Mangano, (C)
2003 O'Reilly & Associates, ISBN 0-596-00372-2. The O'Reilly Policy on
Re-Use of Code Examples from Books
<http://www.oreilly.com/pub/a/oreilly/ask_tim/2001/codepolicy.html>
allows the use of these style sheets in valgrind for this purpose.

The tarball contains:

- File README.txt (this file).

- the stylesheets copy.xslt, docbook2text.xslt, str.dup.xslt,
  str.find-last.xslt, text.justify.xslt, text.wrap.xslt and
  untag-inline.xslt.

- File faq.txt.patch adds the generation of FAQ.txt to file
  docs/Makefile.am. It is based on the assumptions that
  * file FAQ.txt will be generated in directory docs.
  * the stylesheets will be stored in directory docs/lib.
  If a different output width is required (e.g. 80), you can use
  command 
    $(XSLTPROC) $(XSLTPROC_FLAGS) $(libdir)/untag-inline.xslt $(xmldir)/FAQ.xml | \
    $(XSLTPROC) $(XSLTPROC_FLAGS) --stringparam width 80 $(libdir)/docbook2text.xslt - > FAQ.txt

- File FAQ.txt as generated on my system (Debian 3.1).

Software used:
xsltproc was compiled against libxml 20616, libxslt 10112 and libexslt 810
libxslt 10112 was compiled against libxml 20616
libexslt 810 was compiled against libxml 20616


Copyright 2005 Jeroen N. Witmond, jnw@xs4all.nl
GNU GENERAL PUBLIC LICENSE Version 2, June 1991 applies.
