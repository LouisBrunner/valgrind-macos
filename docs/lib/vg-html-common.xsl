<?xml version="1.0"?> <!-- -*- sgml -*- -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/docbook.xsl"/>
<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/chunk-common.xsl"/>
<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/manifest.xsl"/>
<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/chunk-code.xsl"/>


<!-- Note [Dec.1 2005]: the tempate 'chunk-code.xsl' in       -->
<!-- '/usr/share/xml/docbook/stylesheet/nwalsh/1.69.0/html/'  -->
<!-- has been edited to remove 'article' and 'refentry'       -->
<!-- so that separate titlepages don't get generated for them -->
<!-- Note [Dec.1 2005, JRS]: ../1.69.0/.. was changed to      -->
<!-- ../current/.. in docs/Makefile.am, since the latter is a -->
<!-- symlink to the former, at least on SuSE 10.0.            -->

<!-- use UTF-8 encoding -->
<xsl:output method="html" encoding="UTF-8" indent="yes"/>

<!-- set various parameters -->
<xsl:param name="use.id.as.filename" select="'1'"/> 
<xsl:param name="chunker.output.indent" select="'yes'"/>
<!-- set chunking at the chapter level only -->
<xsl:param name="chunk.section.depth" select="'0'"/> 
<!-- set toc-levels -->
<xsl:param name="generate.toc">
set       toc,title
book      toc,title,figure,table,example,equation
chapter   toc,title
section   toc
sect1     toc
sect2     toc
sect3     toc
sect4     nop
sect5     nop
qandaset  toc
qandadiv  toc
appendix  toc,title
article/appendix  nop
article   nop
preface   toc,title
reference toc,title
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
<xsl:template match="@fileref">
  <xsl:value-of select="."/>
</xsl:template>

<!-- end properties common to html + fo ............................... -->


<!-- center everything at the top of a titlepage -->
<xsl:attribute-set name="set.titlepage.recto.style">
  <xsl:attribute name="align">center</xsl:attribute>
</xsl:attribute-set>


<!-- don`t put an expanded set-level TOC, only book titles -->
<xsl:template match="book" mode="toc">
  <xsl:param name="toc-context" select="."/>
  <xsl:choose>
    <xsl:when test="local-name($toc-context) = 'set'">
      <xsl:call-template name="subtoc">
        <xsl:with-param name="toc-context" select="$toc-context"/>
        <xsl:with-param name="nodes" select="foo"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="subtoc">
        <xsl:with-param name="toc-context" select="$toc-context"/>
        <xsl:with-param name="nodes" select="part|reference
                                         |preface|chapter|appendix
                                         |article
                                         |bibliography|glossary|index
                                         |refentry
                                         |bridgehead[$bridgehead.in.toc !=
0]"/>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!-- custom footer -->
<xsl:template name="footer.navigation">
  <xsl:param name="prev" select="/foo"/>
  <xsl:param name="next" select="/foo"/>
  <xsl:param name="nav.context"/>

  <xsl:variable name="home" select="/*[1]"/>
  <xsl:variable name="up" select="parent::*"/>

  <xsl:variable name="row1" select="count($prev) &gt; 0
                                    or count($up) &gt; 0
                                    or count($next) &gt; 0"/>

  <xsl:variable name="row2" select="($prev != 0)
             or (generate-id($home) != generate-id(.) or $nav.context = 'toc')
             or ($chunk.tocs.and.lots != 0 and $nav.context != 'toc')
             or ($next != 0)"/>
  <div>
  <xsl:if test="$row1 or $row2">
   <br />
   <table class="nav" width="100%" cellspacing="3" cellpadding="2" border="0" summary="Navigation footer">
    <xsl:if test="$row1">
     <tr>
      <td rowspan="2" width="40%" align="left">
       <xsl:if test="count($prev)>0">
        <a accesskey="p">
         <xsl:attribute name="href">
          <xsl:call-template name="href.target">
           <xsl:with-param name="object" select="$prev"/>
          </xsl:call-template>
         </xsl:attribute>
         <xsl:text>&#060;&#060;&#160;</xsl:text>
         <xsl:apply-templates select="$prev" mode="object.title.markup"/>
        </a>
       </xsl:if>
       <xsl:text>&#160;</xsl:text>
      </td>
      <td width="20%" align="center">
       <xsl:choose>
        <xsl:when test="count($up)>0">
         <a accesskey="u">
          <xsl:attribute name="href">
           <xsl:call-template name="href.target">
            <xsl:with-param name="object" select="$up"/>
           </xsl:call-template>
          </xsl:attribute>
          <xsl:call-template name="navig.content">
           <xsl:with-param name="direction" select="'up'"/>
          </xsl:call-template>
         </a>
        </xsl:when>
        <xsl:otherwise>&#160;</xsl:otherwise>
       </xsl:choose>
      </td>
      <td rowspan="2" width="40%" align="right">
       <xsl:text>&#160;</xsl:text>
       <xsl:if test="count($next)>0">
        <a accesskey="n">
         <xsl:attribute name="href">
          <xsl:call-template name="href.target">
           <xsl:with-param name="object" select="$next"/>
          </xsl:call-template>
         </xsl:attribute>
         <xsl:apply-templates select="$next" mode="object.title.markup"/>
         <xsl:text>&#160;&#062;&#062;</xsl:text>
        </a>
       </xsl:if>
      </td>
     </tr>
    </xsl:if>
    <xsl:if test="$row2">
     <tr>
      <td width="20%" align="center">
       <xsl:choose>
       <xsl:when test="$home != . or $nav.context = 'toc'">
        <a accesskey="h">
         <xsl:attribute name="href">
          <xsl:call-template name="href.target">
           <xsl:with-param name="object" select="$home"/>
          </xsl:call-template>
         </xsl:attribute>
         <xsl:call-template name="navig.content">
          <xsl:with-param name="direction" select="'home'"/>
         </xsl:call-template>
        </a>
        <xsl:if test="$chunk.tocs.and.lots != 0 and $nav.context != 'toc'">
         <xsl:text>&#160;|&#160;</xsl:text>
        </xsl:if>
       </xsl:when>
       <xsl:otherwise>&#160;</xsl:otherwise>
       </xsl:choose>
       <xsl:if test="$chunk.tocs.and.lots != 0 and $nav.context != 'toc'">
        <a accesskey="t">
         <xsl:attribute name="href">
          <xsl:apply-templates select="/*[1]" mode="recursive-chunk-filename"/>
          <xsl:text>-toc</xsl:text>
          <xsl:value-of select="$html.ext"/>
         </xsl:attribute>
         <xsl:call-template name="gentext">
          <xsl:with-param name="key" select="'nav-toc'"/>
         </xsl:call-template>
        </a>
       </xsl:if>
      </td>
     </tr>
    </xsl:if>
   </table>
  </xsl:if>
 </div>
</xsl:template>


<!-- qandaset styles -->
<!-- these templates have been carefully tweaked to correct the        -->
<!-- horrible mess that docbook makes of dl/dt/dd tags. Edit with care -->
<xsl:template match="qandaset">
  <xsl:variable name="title" select="(blockinfo/title|title)[1]"/>
  <xsl:variable name="toc">
    <xsl:call-template name="dbhtml-attribute">
      <xsl:with-param name="pis" select="processing-instruction('dbhtml')"/>
      <xsl:with-param name="attribute" select="'toc'"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="toc.params">
    <xsl:call-template name="find.path.params">
      <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
    </xsl:call-template>
  </xsl:variable>

  <div class="{name(.)}">
    <xsl:apply-templates select="$title"/>
    <xsl:if test="(contains($toc.params, 'toc') and $toc != '0') or $toc = '1'">
      <xsl:call-template name="process.qanda.toc"/>
    </xsl:if>
    <xsl:apply-templates select="qandaentry|qandadiv"/>
  </div>
</xsl:template>


<xsl:template match="qandadiv">
  <br/>
  <table width="100%" summary="Q and A Div" cellpadding="2" cellspacing="2" border="0">
  <xsl:if test="blockinfo/title|title">
    <tr class="qandadiv">
      <td align="left" valign="top" colspan="2">
        <xsl:call-template name="anchor">
          <xsl:with-param name="conditional" select="0"/>
        </xsl:call-template>
        <xsl:apply-templates select="(blockinfo/title|title)[1]"/>
      </td>
    </tr>
  </xsl:if>

  <xsl:variable name="toc">
    <xsl:call-template name="dbhtml-attribute">
      <xsl:with-param name="pis"
                      select="processing-instruction('dbhtml')"/>
      <xsl:with-param name="attribute" select="'toc'"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="toc.params">
    <xsl:call-template name="find.path.params">
      <xsl:with-param name="table" select="normalize-space($generate.toc)"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:if test="(contains($toc.params, 'toc') and $toc != '0') or $toc = '1'">
    <tr class="toc" colspan="2">
      <td align="left" valign="top" colspan="2">
        <xsl:call-template name="process.myqanda.toc"/>
      </td>
    </tr>
  </xsl:if>
  <xsl:apply-templates select="qandadiv|qandaentry"/>
  </table>
</xsl:template>


<!-- put questions in bold -->
<xsl:template match="question/para">
  <b><xsl:apply-templates/></b>
</xsl:template>

<xsl:template match="question">
  <xsl:variable name="deflabel">
    <xsl:choose>
      <xsl:when test="ancestor-or-self::*[@defaultlabel]">
        <xsl:value-of select="(ancestor-or-self::*[@defaultlabel])[last()]
                              /@defaultlabel"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$qanda.defaultlabel"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <tr><td colspan="2"><xsl:text>&#160;</xsl:text></td></tr>
  <tr class="{name(.)}">
    <td align="left" valign="top">
      <xsl:call-template name="anchor">
        <xsl:with-param name="node" select=".."/>
        <xsl:with-param name="conditional" select="0"/>
      </xsl:call-template>
      <xsl:call-template name="anchor">
        <xsl:with-param name="conditional" select="0"/>
      </xsl:call-template>
      <b>
        <xsl:apply-templates select="." mode="label.markup"/>
        <xsl:if test="$deflabel = 'number' and not(label)">
          <xsl:apply-templates select="." mode="intralabel.punctuation"/>
	      </xsl:if>
      </b>
    </td>
    <td align="left" valign="top">
      <xsl:choose>
        <xsl:when test="$deflabel = 'none' and not(label)">
          <b><xsl:apply-templates select="*[name(.) != 'label']"/></b>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="*[name(.) != 'label']"/>
        </xsl:otherwise>
      </xsl:choose>
    </td>
  </tr>
</xsl:template>


<xsl:template name="process.myqanda.toc">
  <xsl:apply-templates select="qandadiv" mode="qandatoc.mode"/>
  <xsl:apply-templates select="qandaentry" mode="myqandatoc.mode"/>
</xsl:template>


<xsl:template name="process.qanda.toc">
  <xsl:apply-templates select="qandadiv" mode="qandatoc.mode"/>
  <xsl:apply-templates select="qandaentry" mode="qandatoc.mode"/>
</xsl:template>


<xsl:template match="qandadiv" mode="qandatoc.mode">
<dl>
  <dt><xsl:apply-templates select="title" mode="qandatoc.mode"/></dt>
  <xsl:call-template name="process.qanda.toc"/>
</dl>
</xsl:template>


<!-- this one is used at the top of the page -->
<xsl:template match="question" mode="qandatoc.mode">
  <xsl:variable name="firstch" select="(*[name(.)!='label'])[1]"/>
  <xsl:variable name="deflabel">
    <xsl:choose>
      <xsl:when test="ancestor-or-self::*[@defaultlabel]">
        <xsl:value-of select="(ancestor-or-self::*[@defaultlabel])[last()]
                              /@defaultlabel"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$qanda.defaultlabel"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <dd>
    <xsl:apply-templates select="." mode="label.markup"/>
    <xsl:if test="$deflabel = 'number' and not(label)">
      <xsl:apply-templates select="." mode="intralabel.punctuation"/>
    </xsl:if>
    <xsl:text> </xsl:text>
    <a>
      <xsl:attribute name="href">
        <xsl:call-template name="href.target">
          <xsl:with-param name="object" select=".."/>
        </xsl:call-template>
      </xsl:attribute>
      <xsl:value-of select="$firstch"/>
    </a>
  </dd>
</xsl:template>


<!-- this one is used within table cells -->
<xsl:template match="qandaentry" mode="myqandatoc.mode">
  <xsl:apply-templates select="question" mode="myqandatoc.mode"/>
</xsl:template>

<xsl:template match="question" mode="myqandatoc.mode">
  <xsl:variable name="firstch" select="(*[name(.)!='label'])[1]"/>
  <xsl:variable name="deflabel">
    <xsl:choose>
      <xsl:when test="ancestor-or-self::*[@defaultlabel]">
        <xsl:value-of select="(ancestor-or-self::*[@defaultlabel])[last()]
                              /@defaultlabel"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$qanda.defaultlabel"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
    <xsl:apply-templates select="." mode="label.markup"/>
    <xsl:if test="$deflabel = 'number' and not(label)">
      <xsl:apply-templates select="." mode="intralabel.punctuation"/>
    </xsl:if>
    <xsl:text> </xsl:text>
    <a>
      <xsl:attribute name="href">
        <xsl:call-template name="href.target">
          <xsl:with-param name="object" select=".."/>
        </xsl:call-template>
      </xsl:attribute>
      <xsl:value-of select="$firstch"/>
    </a>
  <br />
</xsl:template>


</xsl:stylesheet>
