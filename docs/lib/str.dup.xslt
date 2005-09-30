<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:str="http://www.ora.com/XSLTCookbook/namespaces/strings">

<!-- This file was copied with some adaptations from the examples
supplied with the XSLT Cookbook by Sal Mangano, (C) 2003 O'Reilly &
Associates, ISBN 0-596-00372-2. -->

  <xsl:template name="str:dup">
    <xsl:param name="input"/>
    <xsl:param name="count" select="1"/>

    <xsl:choose>
      <xsl:when test="not($count) or not($input)"/>
      <xsl:when test="$count = 1">
        <xsl:value-of select="$input"/>
      </xsl:when>
      <xsl:otherwise>
        <!-- If $count is odd append an extra copy of input -->
        <xsl:if test="$count mod 2">
          <xsl:value-of select="$input"/>
        </xsl:if>
        <!-- Recursively apply template after doubling input and 
			halving count -->
        <xsl:call-template name="str:dup">
          <xsl:with-param name="input" select="concat($input,$input)"/>
          <xsl:with-param name="count" select="floor($count div 2)"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>
