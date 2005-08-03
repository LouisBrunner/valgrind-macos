<?xml version="1.0"?> <!-- -*- sgml -*- -->
<xsl:stylesheet 
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/docbook.xsl"/>
<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/chunk-common.xsl"/>
<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/manifest.xsl"/>
<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/chunk-code.xsl"/>
<xsl:import href="vg-common.xsl"/>

<!-- use 8859-1 encoding -->
<xsl:output method="html" encoding="ISO-8859-1" indent="yes"/>

<xsl:param name="use.id.as.filename" select="'1'"/> 
<xsl:param name="chunker.output.indent" select="'yes'"/>
<!-- use our custom html stylesheet -->
<xsl:param name="html.stylesheet" select="'vg_basic.css'"/>
<!-- set chunking at the chapter level only -->
<xsl:param name="chunk.section.depth" select="'0'"/> 

<!-- use our custom header -->
<xsl:template name="header.navigation">
  <xsl:param name="prev" select="/foo"/>
  <xsl:param name="next" select="/foo"/>
  <xsl:param name="nav.context"/>

  <xsl:variable name="home" select="/*[1]"/>
  <xsl:variable name="up" select="parent::*"/>

  <xsl:variable name="row1" select="$navig.showtitles != 0"/>
  <xsl:variable name="row2" select="count($prev) &gt; 0
                            or (count($up) &gt; 0 
                            and generate-id($up) != generate-id($home) )
                            or count($next) &gt; 0"/>

<div>
<!-- never show header nav stuff on title page -->
<xsl:if test="count($prev)>0">
 <xsl:if test="$row1 or $row2">
  <table class="nav" width="100%" cellspacing="3" cellpadding="3" border="0" summary="Navigation header">
   <xsl:if test="$row2">
    <tr>
     <!-- prev -->
     <td width="22px" align="center" valign="middle">
      <xsl:if test="count($prev)>0">
       <a accesskey="p">
        <xsl:attribute name="href">
         <xsl:call-template name="href.target">
          <xsl:with-param name="object" select="$prev"/>
         </xsl:call-template>
        </xsl:attribute>
        <img src="images/prev.png" width="18" height="21" border="0">
         <xsl:attribute name="alt">
          <xsl:call-template name="gentext">
           <xsl:with-param name="key">nav-prev</xsl:with-param>
          </xsl:call-template>
         </xsl:attribute>
        </img>
       </a>
      </xsl:if>
     </td>
     <!-- up -->
     <xsl:if test="count($up)>0">
      <td width="25px" align="center" valign="middle">
       <a accesskey="u">
        <xsl:attribute name="href">
         <xsl:call-template name="href.target">
          <xsl:with-param name="object" select="$up"/>
         </xsl:call-template>
        </xsl:attribute>
        <img src="images/up.png" width="21" height="18" border="0">
         <xsl:attribute name="alt">
          <xsl:call-template name="gentext">
           <xsl:with-param name="key">nav-up</xsl:with-param>
          </xsl:call-template>
         </xsl:attribute>
        </img>
       </a>
      </td>
     </xsl:if>
     <!-- home -->
     <xsl:if test="$home != . or $nav.context = 'toc'">
      <td width="31px" align="center" valign="middle">
       <a accesskey="h">
        <xsl:attribute name="href">
         <xsl:call-template name="href.target">
          <xsl:with-param name="object" select="$home"/>
         </xsl:call-template>
        </xsl:attribute>
        <img src="images/home.png" width="27" height="20" border="0">
         <xsl:attribute name="alt">
          <xsl:call-template name="gentext">
           <xsl:with-param name="key">nav-up</xsl:with-param>
          </xsl:call-template>
         </xsl:attribute>
        </img>
       </a>
      </td>
     </xsl:if>
     <!-- chapter|section heading -->
     <th align="center" valign="middle">
       <xsl:apply-templates select="$up" mode="object.title.markup"/>
<!--
      <xsl:choose>
       <xsl:when test="count($up) > 0 and generate-id($up) != generate-id($home)">
        <xsl:apply-templates select="$up" mode="object.title.markup"/>
       </xsl:when>
       <xsl:otherwise>
        <xsl:text>Valgrind User`s Manual</xsl:text>
       </xsl:otherwise>
      </xsl:choose>
-->
     </th>
     <!-- next -->
      <td width="22px" align="center" valign="middle">
        <xsl:if test="count($next)>0">
         <a accesskey="n">
          <xsl:attribute name="href">
           <xsl:call-template name="href.target">
            <xsl:with-param name="object" select="$next"/>
           </xsl:call-template>
          </xsl:attribute>
          <img src="images/next.png" width="18" height="21" border="0">
           <xsl:attribute name="alt">
            <xsl:call-template name="gentext">
             <xsl:with-param name="key">nav-next</xsl:with-param>
            </xsl:call-template>
           </xsl:attribute>
          </img>
         </a>
        </xsl:if>
       </td>
      </tr>
    </xsl:if>
   </table>
 </xsl:if>
</xsl:if>
</div>
</xsl:template>


<!-- our custom footer -->
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

<!-- We don`t like tables with borders -->
<xsl:template match="revhistory" mode="titlepage.mode">
  <xsl:variable name="numcols">
    <xsl:choose>
      <xsl:when test="//authorinitials">3</xsl:when>
      <xsl:otherwise>2</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <table width="100%" border="0" summary="Revision history">
    <tr>
      <th align="left" colspan="{$numcols}">
        <h3>Revision History</h3>
      </th>
    </tr>
    <xsl:apply-templates mode="titlepage.mode">
      <xsl:with-param name="numcols" select="$numcols"/>
    </xsl:apply-templates>
  </table>
</xsl:template>

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

<!-- question and answer set mods -->
<xsl:template match="answer">
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
  <tr class="{name(.)}">
    <td><xsl:text>&#160;</xsl:text></td>
    <td align="left" valign="top">
      <xsl:apply-templates select="*[name(.) != 'label']"/>
    </td>
  </tr>
  <tr><td colspan="2"><xsl:text>&#160;</xsl:text></td></tr>
</xsl:template>

</xsl:stylesheet>
