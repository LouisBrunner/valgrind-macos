<?xml version="1.0"?> <!-- -*- sgml -*- -->
<!DOCTYPE xsl:stylesheet [ <!ENTITY vg-css SYSTEM "vg_basic.css"> ]>

<xsl:stylesheet 
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/docbook.xsl"/>
<xsl:import href="vg-common.xsl"/>

<!-- use 8859-1 encoding -->
<xsl:output method="html" encoding="ISO-8859-1" indent="yes"/>

<!-- we include the css directly when generating one large file -->
<xsl:template name="user.head.content">  
  <style type="text/css" media="screen">
    <xsl:text>&vg-css;</xsl:text>
  </style>
</xsl:template>

<!-- We don't like tables with borders -->
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
        <h4>Revision History</h4>
      </th>
    </tr>
    <xsl:apply-templates mode="titlepage.mode">
      <xsl:with-param name="numcols" select="$numcols"/>
    </xsl:apply-templates>
  </table>
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

