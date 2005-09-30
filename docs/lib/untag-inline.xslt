<?xml version="1.0" encoding="UTF-8"?> <!-- -*- sgml -*- -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:include href="copy.xslt"/>

<xsl:strip-space elements="*"/>

<xsl:output method="xml" encoding="UTF-8" indent="yes"/>

<!-- Preprocess a docbook/xml file, replacing elements that are to be -->
<!-- formatted inline with the corresponding text.  -->

<xsl:template match="literal|computeroutput">
  <xsl:value-of select="text()"/>
</xsl:template>

<xsl:template match="ulink">
  <xsl:variable name="url" select="normalize-space(@url)"/>
  <xsl:variable name="text" select="normalize-space(text())"/>

  <xsl:if test="$text and $text != $url">
    <xsl:text>'</xsl:text><xsl:value-of select="$text"/><xsl:text>' </xsl:text>
  </xsl:if>
  <xsl:text>&lt;</xsl:text><xsl:value-of select="$url"/><xsl:text>&gt;</xsl:text>
</xsl:template>

</xsl:stylesheet>
