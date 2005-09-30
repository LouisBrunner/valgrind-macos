<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:str="http://www.ora.com/XSLTCookbook/namespaces/strings"
  xmlns:text="http://www.ora.com/XSLTCookbook/namespaces/text" extension-element-prefixes="text">

<!-- This file was copied with some adaptations from the examples
supplied with the XSLT Cookbook by Sal Mangano, (C) 2003 O'Reilly &
Associates, ISBN 0-596-00372-2. -->

<xsl:include href="str.dup.xslt"/>

<xsl:template name="text:justify">
  <xsl:param name="value" /> 
  <xsl:param name="width" select="10"/>
  <xsl:param name="align" select=" 'left' "/>

  <!-- Truncate if too long -->  
  <xsl:variable name="output" select="substring($value,1,$width)"/>
  
  <xsl:choose>
    <xsl:when test="$align = 'left'">
      <xsl:value-of select="$output"/>
      <xsl:call-template name="str:dup">
        <xsl:with-param name="input" select=" ' ' "/>
        <xsl:with-param name="count" select="$width - string-length($output)"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="$align = 'right'">
      <xsl:call-template name="str:dup">
        <xsl:with-param name="input" select=" ' ' "/>
        <xsl:with-param name="count" select="$width - string-length($output)"/>
      </xsl:call-template>
      <xsl:value-of select="$output"/>
    </xsl:when>
    <xsl:when test="$align = 'center'">
      <xsl:call-template name="str:dup">
        <xsl:with-param name="input" select=" ' ' "/>
        <xsl:with-param name="count" select="floor(($width - string-length($output)) div 2)"/>
      </xsl:call-template>
      <xsl:value-of select="$output"/>
      <xsl:call-template name="str:dup">
        <xsl:with-param name="input" select=" ' ' "/>
        <xsl:with-param name="count" select="ceiling(($width - string-length($output)) div 2)"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>INVALID ALIGN</xsl:otherwise>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>
