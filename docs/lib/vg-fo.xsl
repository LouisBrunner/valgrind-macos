<?xml version="1.0" encoding="UTF-8"?> <!-- -*- sgml -*- -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
     xmlns:fo="http://www.w3.org/1999/XSL/Format" version="1.0">

<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/fo/docbook.xsl"/>

<!-- set indent = yes while debugging, then change to NO -->
<xsl:output method="xml" indent="no"/>


<!-- passivetex fo extensions: PDF bookmarks and index terms -->
<xsl:param name="use.extensions" select="'1'"/>
<xsl:param name="passivetex.extensions" select="1"/>
<!-- turn draft mode off -->
<xsl:param name="draft.mode" select="'no'"></xsl:param>
<!-- be extra sure we are using single sided -->
<xsl:param name="double.sided" select="'0'"/> 
<!-- output in 'block' mode -->
<xsl:param name="variablelist.as.blocks" select="1"/>
<!-- don't show url separately in ulinks -->
<xsl:param name="ulink.show" select="0"/>
<!-- control generation of tocs -->
<xsl:param name="generate.toc">
set       toc,title
book      toc,title
part      toc,title
chapter   nop,title
section   nop
sect1     nop
sect2     nop
sect3     nop
sect4     nop
sect5     nop
article   nop
book/article nop
article/sect1 nop
appendix  toc,title
preface   toc,title
reference toc,title
qandadiv  toc
qandaset  toc
</xsl:param>

<!-- properties common to html + fo ................................... -->

<!-- we like '1.2 Title' -->
<xsl:param name="section.autolabel" select="'1'"/> 
<xsl:param name="section.label.includes.component.label" select="'1'"/>

<!-- Do not put 'Chapter' at the start of eg 'Chapter 1. Doing This' -->
<xsl:param name="local.l10n.xml" select="document('')"/> 
<l:i18n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0"> 
  <l:l10n language="en"> 
    <l:context name="title-numbered">
      <l:template name="chapter" text="%n.&#160;%t"/>
    </l:context> 
  </l:l10n>
</l:i18n>

<!-- per Bob Stayton: turn off xml:base processing pro tem -->
<!-- should hopefully be fixed in next docbook stylesheets release (1.70) -->
<!-- ensures correct source paths, eg. images/my_img.png -->
<xsl:template match="@fileref">
  <xsl:value-of select="."/>
</xsl:template>

<!-- end properties common to html + fo ............................... -->


<!-- Bug-fix for Suse 10 PassiveTex version -->
<!-- Precompute attribute values 'cos PassiveTex is too stupid: -->
<xsl:attribute-set name="component.title.properties">
  <xsl:attribute name="keep-with-next.within-column">always</xsl:attribute>
  <xsl:attribute name="space-before.optimum">
    <xsl:value-of select="concat($body.font.master, 'pt')"/>
  </xsl:attribute>
  <xsl:attribute name="space-before.minimum">
    <xsl:value-of select="$body.font.master * 0.8"/>
    <xsl:text>pt</xsl:text>
  </xsl:attribute>
  <xsl:attribute name="space-before.maximum">
    <xsl:value-of select="$body.font.master * 1.2"/>
    <xsl:text>pt</xsl:text>
  </xsl:attribute>
  <xsl:attribute name="hyphenate">false</xsl:attribute>
</xsl:attribute-set>

<!-- show links in color -->
<xsl:attribute-set name="xref.properties">
  <xsl:attribute name="color">blue</xsl:attribute>
</xsl:attribute-set>

<!-- colored background for programlisting and screen -->
<!-- setting param shade.verbatim=1 screws up literallayout -->
<!-- something chronic, so have to go this route -->
<xsl:template match="programlisting|screen|synopsis">
  <xsl:param name="suppress-numbers" select="'0'"/>
  <xsl:variable name="id"><xsl:call-template name="object.id"/></xsl:variable>
  <xsl:variable name="content">
    <xsl:choose>
      <xsl:when test="$suppress-numbers = '0'
                      and @linenumbering = 'numbered'
                      and $use.extensions != '0'
                      and $linenumbering.extension != '0'">
        <xsl:call-template name="number.rtf.lines">
          <xsl:with-param name="rtf">
            <xsl:apply-templates/>
          </xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <fo:block id="{$id}" white-space-collapse='false' white-space-treatment='preserve'
            linefeed-treatment="preserve"  background-color="#f2f2f9"
            xsl:use-attribute-sets="monospace.verbatim.properties">
    <xsl:choose>
      <xsl:when test="$hyphenate.verbatim != 0 
                      and function-available('exsl:node-set')">
        <xsl:apply-templates select="exsl:node-set($content)" 
                             mode="hyphenate.verbatim"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="$content"/>
      </xsl:otherwise>
    </xsl:choose>
  </fo:block>
</xsl:template>


<!-- customised set title-page template -->
<xsl:template name="set.titlepage">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format">
    <xsl:variable name="recto.content">
      <xsl:apply-templates mode="set.titlepage.recto.auto.mode" 
                           select="setinfo/title"/>
      <xsl:apply-templates mode="set.titlepage.recto.auto.mode" 
                           select="setinfo/releaseinfo"/>
      <xsl:apply-templates mode="set.titlepage.recto.auto.mode" 
                           select="setinfo/copyright"/>
      <xsl:apply-templates mode="set.titlepage.recto.auto.mode" 
                           select="setinfo/legalnotice"/>
    </xsl:variable>
    <xsl:variable name="recto.elements.count" select="1"/>
      <fo:block>
        <xsl:copy-of select="$recto.content"/>
      </fo:block>
    <xsl:call-template name="set.titlepage.separator"/>
  </fo:block>
</xsl:template>

<!-- put some extra space after the set title -->
<xsl:template match="title" mode="set.titlepage.recto.auto.mode">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" 
            xsl:use-attribute-sets="set.titlepage.recto.style" 
            text-align="center" font-size="24.8832pt" space-before="18.6624pt" 
            space-after="18.6624pt" font-weight="bold" font-family="{$title.fontset}">
    <xsl:call-template name="division.title">
      <xsl:with-param name="node" select="ancestor-or-self::set[1]"/>
    </xsl:call-template>
  </fo:block>
</xsl:template>

<!-- put release-info + copyright centered and bold -->
<xsl:template match="releaseinfo" mode="set.titlepage.recto.auto.mode">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" text-align="center" 
            font-size="14pt" font-weight="bold" font-family="{$title.fontset}">
    <xsl:apply-templates select="." mode="set.titlepage.recto.mode"/>
  </fo:block>
</xsl:template>

<xsl:template match="copyright" mode="set.titlepage.recto.auto.mode">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" text-align="center" 
            font-size="12pt" font-weight="bold" font-family="{$title.fontset}">
    <xsl:apply-templates select="." mode="set.titlepage.recto.mode"/>
  </fo:block>
</xsl:template>


<!-- customised book title-page template -->
<xsl:template name="book.titlepage">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format">
    <xsl:variable name="recto.content">
      <xsl:apply-templates mode="book.titlepage.recto.auto.mode" 
                           select="bookinfo/title"/>
      <xsl:apply-templates mode="book.titlepage.recto.auto.mode" 
                           select="bookinfo/subtitle"/>
      <xsl:apply-templates mode="book.titlepage.recto.auto.mode" 
                           select="bookinfo/releaseinfo"/>
      <xsl:apply-templates mode="book.titlepage.recto.auto.mode" 
                           select="bookinfo/copyright"/>
      <xsl:apply-templates mode="book.titlepage.recto.auto.mode" 
                           select="bookinfo/author"/>
      <xsl:apply-templates mode="book.titlepage.recto.auto.mode" 
                           select="bookinfo/legalnotice"/>
    </xsl:variable>
    <xsl:variable name="recto.elements.count" select="1"/>
    <fo:block>
      <xsl:copy-of select="$recto.content"/>
    </fo:block>
    <xsl:call-template name="book.titlepage.separator"/>
  </fo:block>
</xsl:template>

<!-- put some extra space after the book title -->
<xsl:template match="title" mode="book.titlepage.recto.auto.mode">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" 
            xsl:use-attribute-sets="book.titlepage.recto.style" 
            text-align="center" font-size="24.8832pt" space-before="18.6624pt" 
            space-after="18.6624pt" font-weight="bold" font-family="{$title.fontset}">
    <xsl:call-template name="division.title">
      <xsl:with-param name="node" select="ancestor-or-self::book[1]"/>
    </xsl:call-template>
  </fo:block>
</xsl:template>

<!-- center subtitles -->
<xsl:template match="subtitle" mode="book.titlepage.recto.auto.mode">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" text-align="center"
            font-size="14pt" font-weight="bold" font-family="{$title.fontset}">
    <xsl:apply-templates select="." mode="book.titlepage.recto.mode"/>
  </fo:block>
</xsl:template>

<!-- no docbook template for bookinfo/releaseinfo, so make one -->
<xsl:template match="releaseinfo" mode="book.titlepage.recto.auto.mode">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" text-align="center"
           font-size="12pt" font-weight="bold" font-family="{$title.fontset}">
    <xsl:apply-templates select="." mode="book.titlepage.recto.mode"/>
  </fo:block>
</xsl:template>

<!-- no docbook recto template for bookinfo/copyright, so make one -->
<xsl:template match="copyright" mode="book.titlepage.recto.auto.mode">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" text-align="center"
            font-size="11pt" font-weight="bold" font-family="{$title.fontset}">
    <xsl:apply-templates select="." mode="book.titlepage.recto.mode"/>
  </fo:block>
</xsl:template>

<!-- customised email for titlepages -->
<xsl:template match="author" mode="book.titlepage.recto.auto.mode">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" text-align="center"
            font-size="10pt" font-family="{$title.fontset}">
    <xsl:if test="email">
      <xsl:text>Email: </xsl:text>
      <xsl:apply-templates select="(email)[1]"/>
    </xsl:if>
  </fo:block>
</xsl:template>


<!-- show only book titles in the top-level set toc -->
<xsl:template match="book|setindex" mode="toc">
  <xsl:param name="toc-context" select="."/>
  <xsl:variable name="id">
    <xsl:call-template name="object.id"/>
  </xsl:variable>
  <xsl:variable name="cid">
    <xsl:call-template name="object.id">
      <xsl:with-param name="object" select="$toc-context"/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:call-template name="toc.line"/>
  <xsl:variable name="nodes" select="glossary|bibliography|preface|chapter
                                     |reference|part|article|appendix|index"/>
  <xsl:variable name="depth.from.context" 
                select="count(ancestor::*)-count($toc-context/ancestor::*)"/>
  <xsl:if test="$toc.section.depth > 0 and not($toc-context/self::set)
                and $toc.max.depth > $depth.from.context and $nodes">
    <fo:block id="toc.{$cid}.{$id}">
      <xsl:attribute name="margin-left">
        <xsl:call-template name="set.toc.indent"/>
      </xsl:attribute>
      <xsl:apply-templates select="$nodes" mode="toc">
        <xsl:with-param name="toc-context" select="$toc-context"/>
      </xsl:apply-templates>
    </fo:block>
  </xsl:if>
</xsl:template>


<!-- TODO: don`t generate a TOC at all for Quick-Start and FAQ -->



<!-- page headers: -->
<xsl:template name="header.table">
  <xsl:param name="pageclass" select="''"/>
  <xsl:param name="sequence" select="''"/>
  <xsl:param name="gentext-key" select="''"/>
  <xsl:variable name="candidate">
    <fo:table table-layout="fixed" width="100%" border-bottom-width="0.5pt" border-bottom-style="solid" border-bottom-color="black">
    <fo:table-column column-number="1" column-width="100%"/>
    <fo:table-body>
      <fo:table-row height="14pt">
        <fo:table-cell text-align="right" display-align="before" relative-align="baseline">
          <fo:block>
            <xsl:apply-templates select="." mode="titleabbrev.markup"/>
          </fo:block>
        </fo:table-cell>
      </fo:table-row>
    </fo:table-body>
    </fo:table>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$pageclass='titlepage' or $sequence='blank' 
                   or ($sequence='first' and $gentext-key='chapter')">
      <!-- noop on titlepages, first chapter page or blank pages -->
    </xsl:when>
    <xsl:otherwise>
      <xsl:copy-of select="$candidate"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<!-- page footers: two 80|20 cols w/right-aligned page numbers -->
<xsl:template name="footer.table">
  <xsl:param name="pageclass" select="''"/>
  <xsl:param name="sequence" select="''"/>
  <xsl:param name="gentext-key" select="''"/>
  <xsl:variable name="candidate">
    <fo:table table-layout="fixed" width="100%">
    <fo:table-column column-number="1" column-width="80%"/>
    <fo:table-column column-number="2" column-width="20%"/>
    <fo:table-body>
      <fo:table-row height="14pt">
        <fo:table-cell text-align="left" display-align="after" 
                       relative-align="baseline">
          <fo:block/>
<!--
          <fo:block>
            <xsl:text>Pageclass: </xsl:text>
            <xsl:value-of select="$pageclass"/>
            <xsl:text>  Sequence: </xsl:text>
            <xsl:value-of select="$sequence"/>
            <xsl:text>  Gentext-Key: </xsl:text>
            <xsl:value-of select="$gentext-key"/>
          </fo:block>
-->
        </fo:table-cell>
        <fo:table-cell text-align="right" display-align="after" 
                       relative-align="baseline">
          <fo:block>
            <fo:page-number/>
          </fo:block>
        </fo:table-cell>
      </fo:table-row>
    </fo:table-body>
    </fo:table>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="$pageclass='titlepage' or $sequence='blank'">
      <!-- noop on titlepages or blank pages -->
    </xsl:when>
    <xsl:otherwise>
      <xsl:copy-of select="$candidate"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!-- workaround bug in passivetex fo output for itemizedlist -->
<xsl:template match="itemizedlist/listitem">
  <xsl:variable name="id"><xsl:call-template name="object.id"/></xsl:variable>
  <xsl:variable name="item.contents">
    <fo:list-item-label end-indent="label-end()">
      <fo:block>
        <xsl:call-template name="itemizedlist.label.markup">
          <xsl:with-param name="itemsymbol">
            <xsl:call-template name="list.itemsymbol">
              <xsl:with-param name="node" select="parent::itemizedlist"/>
            </xsl:call-template>
          </xsl:with-param>
        </xsl:call-template>
      </fo:block>
    </fo:list-item-label>
    <fo:list-item-body start-indent="body-start()">
      <xsl:apply-templates/>    <!-- removed extra block wrapper -->
    </fo:list-item-body>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="parent::*/@spacing = 'compact'">
      <fo:list-item id="{$id}" xsl:use-attribute-sets="compact.list.item.spacing">
        <xsl:copy-of select="$item.contents"/>
      </fo:list-item>
    </xsl:when>
    <xsl:otherwise>
      <fo:list-item id="{$id}" xsl:use-attribute-sets="list.item.spacing">
        <xsl:copy-of select="$item.contents"/>
      </fo:list-item>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- workaround bug in passivetex fo output for orderedlist -->
<xsl:template match="orderedlist/listitem">
  <xsl:variable name="id"><xsl:call-template name="object.id"/></xsl:variable>
  <xsl:variable name="item.contents">
    <fo:list-item-label end-indent="label-end()">
      <fo:block>
        <xsl:apply-templates select="." mode="item-number"/>
      </fo:block>
    </fo:list-item-label>
    <fo:list-item-body start-indent="body-start()">
      <xsl:apply-templates/>    <!-- removed extra block wrapper -->
    </fo:list-item-body>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="parent::*/@spacing = 'compact'">
      <fo:list-item id="{$id}" xsl:use-attribute-sets="compact.list.item.spacing">
        <xsl:copy-of select="$item.contents"/>
      </fo:list-item>
    </xsl:when>
    <xsl:otherwise>
      <fo:list-item id="{$id}" xsl:use-attribute-sets="list.item.spacing">
        <xsl:copy-of select="$item.contents"/>
      </fo:list-item>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


</xsl:stylesheet>
