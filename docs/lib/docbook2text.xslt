<?xml version="1.0" encoding="UTF-8"?> <!-- -*- sgml -*- -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:text="http://www.ora.com/XSLTCookbook/namespaces/text"
  xmlns:str="http://www.ora.com/XSLTCookbook/namespaces/strings">

<xsl:include href="text.wrap.xslt"/>

<xsl:strip-space elements="*"/>
<xsl:preserve-space elements="screen programlisting"/>
<xsl:output method="text"/>

<!-- Transform a subset of docbook/xml into plain text. This -->
<!-- stylesheet assumes that all elements that are to be formatted -->
<!-- inline already have been handled by untag-inline.xslt. -->

<!-- Maximum number of characters on one line. --> 
<xsl:param name="width" select="72"/>

<!-- This stylesheet uses two named templates. The template named -->
<!-- "header" writes its input, followed by a line of '~'s of -->
<!-- the same length. The template named "ruler" produces a line of -->
<!-- $width '-'s. They are implemented at the bottom of this file. -->

<xsl:template match="/">
  <!-- Title 'page' stuff. -->
  <xsl:call-template name="header">
    <xsl:with-param name="input" select="book/title"/>
  </xsl:call-template>

  <xsl:call-template name="header">
    <xsl:with-param name="input" select="book/bookinfo/releaseinfo"/>
  </xsl:call-template>
  <xsl:text>&#xa;</xsl:text>

  <!-- List of sections. -->
  <xsl:text>Table of Contents&#xa;</xsl:text>
  <xsl:apply-templates select="book/sect1" mode="toc"/>
  <xsl:text>&#xa;</xsl:text>

  <!-- Contents. -->
  <xsl:apply-templates select="book/sect1"/>
</xsl:template>

<xsl:template match="sect1" mode="toc">
  <xsl:value-of select="position()"/>
  <xsl:text>. </xsl:text>
  <xsl:value-of select="title"/>
  <xsl:text>&#xa;</xsl:text>
</xsl:template>

<!-- Processing of the contents starts here. -->

<xsl:template match="sect1">
  <!-- The FAQ contains two types of sect1: those containing a
    qandaset (actually an unspecified number), and those containing a
    sequence of paras. To get the number of blank lines right, these
    must be treated separately. -->

  <xsl:variable name="sectno" select="position()"/>

  <xsl:call-template name="ruler"/>
  <xsl:value-of select="$sectno"/>
  <xsl:text>. </xsl:text>
  <xsl:value-of select="title"/>
  <xsl:text>&#xa;</xsl:text>
  <xsl:call-template name="ruler"/>
  <xsl:text>&#xa;</xsl:text>

  <xsl:choose>
    <xsl:when test="qandaset">
      <xsl:apply-templates select="qandaset|para">
        <xsl:with-param name="sectno" select="$sectno"/>
      </xsl:apply-templates>
    </xsl:when>

    <xsl:when test="para">
      <xsl:for-each select="para|screen|programlisting|itemizedlist|orderedlist">
        <xsl:apply-templates select="."/>

        <xsl:if test="position() != last()">
            <xsl:text>&#xa;</xsl:text>
        </xsl:if>
      </xsl:for-each>
    </xsl:when>

    <!-- Oops. sect1 contains elements we do not yet handle. -->
    <xsl:otherwise>
      <xsl:message>template match="sect1": Encountered
      &lt;<xsl:value-of select="name(.)"/>&gt;.</xsl:message>
    </xsl:otherwise>
  </xsl:choose>

  <xsl:text>&#xa;</xsl:text>
</xsl:template>

<xsl:template match="qandaset">
  <xsl:param name="sectno"/>

  <xsl:apply-templates select="qandaentry">
    <xsl:with-param name="sectno" select="$sectno"/>
  </xsl:apply-templates>
</xsl:template>

<xsl:template match="qandaentry">
  <xsl:param name="sectno"/>

  <xsl:variable name="questno" select="position()"/>

  <xsl:variable name="prefix">
    <xsl:value-of select="$sectno"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="$questno"/>
    <xsl:text>. </xsl:text>
  </xsl:variable>
  <xsl:variable name="prefix-length" select="string-length($prefix)"/>

  <xsl:if test="$questno > 1">
    <xsl:text>&#xa;</xsl:text>
    <xsl:call-template name="ruler"/>
    <xsl:text>&#xa;</xsl:text>
  </xsl:if>

  <xsl:apply-templates select="question">
    <xsl:with-param name="prefix" select="$prefix"/>
    <xsl:with-param name="indent" select="$prefix-length"/>
  </xsl:apply-templates>

  <xsl:text>&#xa;</xsl:text>

  <xsl:apply-templates select="answer"/>
</xsl:template>

<xsl:template match="question">
  <xsl:param name="prefix"/>
  <xsl:param name="indent" select="0"/>

  <xsl:for-each select="para|screen|programlisting">
    <xsl:choose>
      <xsl:when test="position() = 1 and $prefix">
        <xsl:value-of select="$prefix"/>
        <xsl:apply-templates select=".">
          <xsl:with-param name="indent" select="$indent"/>
          <xsl:with-param name="dofirst" select="0"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select=".">
          <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>

    <xsl:if test="position() != last()">
        <xsl:text>&#xa;</xsl:text>
    </xsl:if>
  </xsl:for-each>
</xsl:template>

<xsl:template match="answer">
  <xsl:param name="indent" select="0"/>

  <xsl:for-each select="para|screen|programlisting|itemizedlist|orderedlist">
    <xsl:apply-templates select=".">
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:apply-templates>

    <xsl:if test="position() != last()">
        <xsl:text>&#xa;</xsl:text>
    </xsl:if>
  </xsl:for-each>
</xsl:template>

<xsl:template match="para">
  <xsl:param name="indent" select="0"/>
  <xsl:param name="dofirst" select="1"/>

  <xsl:for-each select="node()">
    <xsl:choose>
      <!-- Lists and blocks as children. -->
      <xsl:when test="self::screen|self::programlisting|
		      self::itemizedlist|self::orderedlist">
        <xsl:apply-templates select=".">
          <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>

        <xsl:if test="position() != last()">
            <xsl:text>&#xa;</xsl:text>
        </xsl:if>
      </xsl:when>

      <!-- Text. Inline elements have been flattened by
           untag-inline.xslt. -->
      <xsl:when test="self::text()">
        <xsl:choose>
          <xsl:when test="position() = 1 and $dofirst = 0">
            <xsl:apply-templates select="." mode="text:wrap">
              <xsl:with-param name="width" select="$width"/>
              <xsl:with-param name="indent" select="$indent"/>
              <xsl:with-param name="dofirst" select="0"/>
            </xsl:apply-templates>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates select="." mode="text:wrap">
              <xsl:with-param name="width" select="$width"/>
              <xsl:with-param name="indent" select="$indent"/>
            </xsl:apply-templates>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>

      <!-- Oops. para contains elements we do not yet handle. -->
      <xsl:otherwise>
        <xsl:message>template match="para": Encountered
        &lt;<xsl:value-of select="name(.)"/>&gt;.</xsl:message>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>
</xsl:template>

<xsl:template match="screen|programlisting">
  <xsl:param name="indent" select="0"/>
  <xsl:param name="dofirst" select="1"/><!-- ignored. -->

  <xsl:variable name="myindent" select=" $indent + 2 " />

  <xsl:apply-templates select="." mode="text:dump">
    <xsl:with-param name="input" select="text()"/>
    <xsl:with-param name="indent" select="$myindent"/>
  </xsl:apply-templates>
</xsl:template>

<xsl:template match="itemizedlist">
  <xsl:param name="indent" select="0"/>

  <xsl:variable name="prefix" select=" '* ' "/>
  <xsl:variable name="prefix-length" select="string-length($prefix)"/>

  <xsl:for-each select="listitem">
    <xsl:apply-templates select=".">
      <xsl:with-param name="prefix" select="$prefix"/>
      <xsl:with-param name="indent" select="$prefix-length"/>
    </xsl:apply-templates>

    <xsl:if test="position() != last()">
        <xsl:text>&#xa;</xsl:text>
    </xsl:if>
  </xsl:for-each>
</xsl:template>

<xsl:template match="orderedlist">
  <xsl:param name="indent" select="0"/>

  <xsl:for-each select="listitem">
    <xsl:variable name="prefix">
      <xsl:value-of select="position()"/>
      <xsl:text>. </xsl:text>
    </xsl:variable>
    <xsl:variable name="prefix-length" select="string-length($prefix)"/>

    <xsl:apply-templates select=".">
      <xsl:with-param name="prefix" select="$prefix"/>
      <xsl:with-param name="indent" select="$prefix-length"/>
    </xsl:apply-templates>

    <xsl:if test="position() != last()">
        <xsl:text>&#xa;</xsl:text>
    </xsl:if>
  </xsl:for-each>
</xsl:template>

<xsl:template match="listitem">
  <xsl:param name="prefix"/>
  <xsl:param name="indent" select="0"/>

  <xsl:for-each select="para|screen|programlisting">
    <xsl:choose>
      <xsl:when test="position() = 1 and $prefix">
        <xsl:value-of select="$prefix"/>
        <xsl:apply-templates select=".">
          <xsl:with-param name="indent" select="$indent"/>
          <xsl:with-param name="dofirst" select="0"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select=".">
          <xsl:with-param name="indent" select="$indent"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>
</xsl:template>

<!-- Named utility templates. -->

<xsl:template name="header">
  <xsl:param name="input" select="0"/>
  <xsl:variable name="input-length" select="string-length($input)"/>

  <xsl:value-of select="$input"/>
  <xsl:text>&#xa;</xsl:text>

  <xsl:call-template name="str:dup">
    <xsl:with-param name="input" select=" '~' "/>
    <xsl:with-param name="count" select="$input-length"/>
  </xsl:call-template>
  <xsl:text>&#xa;</xsl:text>
</xsl:template>

<xsl:template name="ruler">
  <xsl:call-template name="str:dup">
    <xsl:with-param name="input" select=" '-' "/>
    <xsl:with-param name="count" select="$width"/>
  </xsl:call-template>
  <xsl:text>&#xa;</xsl:text>
</xsl:template>

</xsl:stylesheet>
