<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="text.wrap"
  xmlns:str="http://www.ora.com/XSLTCookbook/namespaces/strings" 
  xmlns:text="http://www.ora.com/XSLTCookbook/namespaces/text" exclude-result-prefixes="text">

<!-- This file was copied with some adaptations from the examples
supplied with the XSLT Cookbook by Sal Mangano, (C) 2003 O'Reilly &
Associates, ISBN 0-596-00372-2. -->

<xsl:include href="str.find-last.xslt"/>
<xsl:include href="text.justify.xslt"/>

<xsl:template match="node() | @*" mode="text:wrap" name="text:wrap">
  <xsl:param name="input" select="normalize-space()"/> 
  <xsl:param name="width" select="70"/>
  <xsl:param name="indent" select="0"/>
  <xsl:param name="dofirst" select="1"/>
  <xsl:param name="align" select=" 'left' "/>

  <xsl:variable name="effective" select="$width - $indent"/>

  <xsl:if test="$input">
    <xsl:variable name="line">
      <xsl:choose>
        <xsl:when test="string-length($input) > $effective">
          <xsl:variable name="candidate-line" select="substring($input,1,$effective)"/>
          <xsl:choose>
            <xsl:when test="contains($candidate-line,' ')">
              <xsl:call-template name="str:substring-before-last">
                  <xsl:with-param name="input" select="$candidate-line"/>
                  <xsl:with-param name="substr" select=" ' ' "/>
              </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$candidate-line"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$input"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
  
    <xsl:if test="$line">
      <xsl:if test="$dofirst">
        <xsl:call-template name="str:dup">
          <xsl:with-param name="input" select=" ' ' "/>
          <xsl:with-param name="count" select="$indent"/>
        </xsl:call-template>
      </xsl:if>

      <xsl:call-template name="text:justify">
        <xsl:with-param name="value" select="$line"/>
        <xsl:with-param name="width" select="$effective"/>
        <xsl:with-param name="align" select="$align"/>
      </xsl:call-template>
      <xsl:text>&#xa;</xsl:text>
    </xsl:if>  

    <xsl:call-template name="text:wrap">
      <xsl:with-param name="input" select="substring($input, string-length($line) + 2)"/>
      <xsl:with-param name="width" select="$width"/>
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="align" select="$align"/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template match="node() | @*" mode="text:dump" name="text:dump">
  <xsl:param name="input" select="text()"/>
  <xsl:param name="indent" select="0"/>

  <xsl:if test="$input">
    <xsl:variable name="line">
      <xsl:choose>
        <xsl:when test="contains($input, '&#xA;')">
          <xsl:value-of select="substring-before($input, '&#xA;')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$input"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
  
    <xsl:if test="$line">
      <xsl:call-template name="str:dup">
        <xsl:with-param name="input" select=" ' ' "/>
        <xsl:with-param name="count" select="$indent"/>
      </xsl:call-template>
      <xsl:value-of select="$line"/>
      <xsl:text>&#xa;</xsl:text>
    </xsl:if>

    <xsl:call-template name="text:dump">
      <xsl:with-param name="input" select="substring-after($input, '&#xA;')"/>
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>
