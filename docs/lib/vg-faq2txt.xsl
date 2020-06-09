<?xml version="1.0" encoding="ISO-8859-1"?> <!-- -*- sgml -*- -->
<!DOCTYPE xsl:stylesheet [ 
  <!ENTITY nl "&#xa;"> 
  <!ENTITY line-len "72">
  <!ENTITY indent "    ">
  <!-- we always output the same-length rule, so just define it as an entity -->
  <!ENTITY rule "------------------------------------------------------------------------">
]>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/fo/docbook.xsl"/>


<!-- line-wrapping stuff -->
<xsl:import href="line-wrap.xsl"/>

<!-- dump all white-space only nodes -->
<xsl:strip-space elements="*"/>


<xsl:output method="text" encoding="UTF-8"
            indent="yes" omit-xml-declaration="yes"/>

<xsl:param name="generate.toc">
book      nop
article   nop
qandaset  toc
qandadiv  nop
</xsl:param>


<!-- top-level place to start -->
<xsl:template match="*" mode="process.root">
  <xsl:text>&nl;</xsl:text>
  <xsl:apply-templates select="."/>
</xsl:template>


<!-- book templates -->
<xsl:template match="book">
  <xsl:call-template name="book.titlepage"/>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template name="book.titlepage">
  <xsl:text>&nl;</xsl:text>
  <xsl:variable name="recto.content">
    <xsl:apply-templates mode="book.titlepage.recto.auto.mode" 
                         select="bookinfo/title"/>
    <xsl:apply-templates mode="book.titlepage.recto.auto.mode" 
                         select="bookinfo/releaseinfo"/>
    <xsl:apply-templates mode="book.titlepage.recto.auto.mode" 
                         select="bookinfo/copyright"/>
    <xsl:apply-templates mode="book.titlepage.recto.auto.mode" 
                         select="bookinfo/author"/>
  </xsl:variable>
    <xsl:copy-of select="$recto.content"/>

</xsl:template>

<xsl:template match="title" mode="book.titlepage.recto.auto.mode">
  <xsl:value-of select="."/>
  <xsl:text>&nl;</xsl:text>
</xsl:template>

<xsl:template match="releaseinfo" mode="book.titlepage.recto.auto.mode">
  <xsl:value-of select="."/>
  <xsl:text>&nl;</xsl:text>

  <xsl:call-template name="ruler">
    <xsl:with-param name="char" select="'~'"/>
    <xsl:with-param name="len" select="string-length(.)"/>
  </xsl:call-template>

  <xsl:text>&nl;</xsl:text>
</xsl:template>

<!-- noop -->
<xsl:template match="copyright" mode="book.titlepage.recto.auto.mode"/>
<!-- noop -->
<xsl:template match="author" mode="book.titlepage.recto.auto.mode"/>


<!-- article templates -->
<xsl:template match="article">
  <xsl:call-template name="article.titlepage"/>
  <xsl:apply-templates/>
</xsl:template>

<!-- noop -->
<xsl:template name="article.titlepage"/>
<!-- noop -->
<xsl:template match="article/title" mode="titlepage.mode"/>



<!-- qandaset / qandadiv / qandaentry templates -->
<xsl:template match="qandaset">
  <xsl:variable name="title" select="(title)[1]"/>
    <xsl:text>Table of Contents</xsl:text>
    <xsl:apply-templates select="$title"/>
    <xsl:call-template name="process.qanda.toc"/>
    <xsl:text>&nl;</xsl:text>
    <!-- do the thang -->
    <xsl:apply-templates select="qandaentry|qandadiv"/>
</xsl:template>

<xsl:template match="qandadiv/title">
  <xsl:variable name="qalevel">
    <xsl:call-template name="qandadiv.section.level"/>
  </xsl:variable>

  <xsl:text>&nl;&rule;&nl;</xsl:text>
  <xsl:apply-templates select="parent::qandadiv" mode="label.markup"/>
  <xsl:text>. </xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&nl;&rule;&nl;&nl;</xsl:text>
</xsl:template>

<xsl:template name="process.qanda.toc">
  <xsl:apply-templates select="qandadiv" mode="qandatoc.mode"/>
</xsl:template>

<xsl:template match="qandadiv" mode="qandatoc.mode">
  <xsl:apply-templates select="title" mode="qandatoc.mode"/>
  <xsl:call-template name="process.qanda.toc"/>
</xsl:template>

<xsl:template match="qandadiv/title" mode="qandatoc.mode">
  <xsl:variable name="qalevel">
    <xsl:call-template name="qandadiv.section.level"/>
  </xsl:variable>

  <xsl:text>&nl;</xsl:text>
  <xsl:apply-templates select="parent::qandadiv" mode="label.markup"/>
  <xsl:text>. </xsl:text>

  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="qandadiv">
  <xsl:apply-templates select="(title)[1]"/>
  <xsl:apply-templates select="qandadiv|qandaentry"/>
</xsl:template>

<!-- if this qandaentry is not the first child in the parent qandadiv, -->
<!-- then output a separator line -->
<xsl:template match="qandaentry">
  <xsl:if test="position() > 1">
    <xsl:text>&rule;&nl;&nl;</xsl:text>
  </xsl:if>
  <xsl:apply-templates/>
</xsl:template>



<!-- TODO: implement indenting of subsequent lines in question para -->
<!-- maybe pass a var 'indent' along to foldl ... ... -->
<xsl:template match="question">
  <xsl:variable name="qnum">
    <xsl:apply-templates select="." mode="label.markup"/>
  </xsl:variable>

  <xsl:for-each select="para|screen|programlisting|itemizedlist|orderedlist">
    <xsl:choose>
      <!-- glue $qnum onto the front of the first question para -->
      <xsl:when test="local-name() = 'para' and position() = 1">
        <xsl:apply-templates select=".">
          <xsl:with-param name="prefix" select="concat($qnum, '. ')"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select=".">
          <xsl:with-param name="prefix" select="zzzz"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>

</xsl:template>


<xsl:template match="answer">
  <xsl:apply-templates select="*[name(.) != 'label']"/>
</xsl:template>


<xsl:template match="para">
  <xsl:param name="prefix" select="''"/>

  <!-- reformat any ulinks found -->
  <xsl:variable name="text">
    <xsl:choose>
      <xsl:when test="descendant-or-self::ulink">
        <xsl:apply-templates name="ulink"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <!-- the input to str-split-to-lines must be delim'd by either   -->
  <!-- a space or a newline. since we need a newline at the end of -->
  <!-- every para anyway, stick it on now -->
  <xsl:variable name="pLine" 
                select="concat($prefix, normalize-space($text), '&nl;')"/>

  <xsl:choose>
    <xsl:when test="string-length($pLine) &lt; &line-len;">
      <xsl:value-of select="$pLine"/>
    </xsl:when>
    <xsl:otherwise>
      <!-- TODO: this is where an 'indent' param should be sent -->
      <xsl:call-template name="str-split-to-lines">
        <xsl:with-param name="pStr" select="$pLine"/>
        <xsl:with-param name="pLineLength" select="&line-len;"/>
        <xsl:with-param name="pDelimiters" select="' &nl;'"/>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>&nl;</xsl:text>
</xsl:template>


<!-- always indent the contents of screen / programlisting -->
<xsl:template match="screen|programlisting">
  <xsl:call-template name="indent.me">
    <xsl:with-param name="first" select="'1'"/>
  </xsl:call-template>
  <xsl:text>&nl;</xsl:text>
</xsl:template>


<xsl:template match="itemizedlist">
  <xsl:apply-templates select="listitem"/>
</xsl:template>

<xsl:template match="itemizedlist/listitem">
  <xsl:apply-templates>
    <xsl:with-param name="prefix" select="'* '"/>
  </xsl:apply-templates>
</xsl:template>


<xsl:template match="orderedlist">
  <xsl:apply-templates select="listitem"/>
</xsl:template>

<xsl:template match="orderedlist/listitem">
  <xsl:apply-templates>
    <xsl:with-param name="prefix" select="concat(position(), '. ' )"/>
  </xsl:apply-templates>
</xsl:template>


<xsl:template match="ulink" name="ulink">
  <xsl:variable name="url"  select="normalize-space(@url)"/>

  <xsl:value-of select="concat(text(), ': ')"/>

  <xsl:text>&lt;</xsl:text>
  <xsl:choose>
    <xsl:when test="starts-with($url, 'mailto:')">
      <xsl:value-of select="substring-after($url, ':')"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$url"/>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>&gt;</xsl:text>

</xsl:template>


<!-- indent a string -->
<xsl:template name="indent.me">
  <xsl:param name="text" select="."/>
  <xsl:param name="first" select="'0'"/>
  <xsl:choose>
    <xsl:when test="contains($text, '&#xa;')">
      <xsl:value-of select="substring-before($text, '&#xa;')"/>
      <xsl:if test="$first = 0">
        <xsl:text>&nl;</xsl:text>
      </xsl:if>
      <xsl:text>&indent;</xsl:text>
      <xsl:call-template name="indent.me">
        <xsl:with-param name="text" select="substring-after($text, '&#xa;')"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:if test="not(string-length($text) = 0)">
        <xsl:text>&indent;</xsl:text>
	      <xsl:value-of select="$text"/>
        <xsl:text>&nl;</xsl:text>
      </xsl:if>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<!-- Repeat the character 'char' 'len' times -->
<xsl:template name="ruler">
  <xsl:param name="char"/>
  <xsl:param name="len"/>
  <xsl:param name="line"/>
  <xsl:choose>
    <xsl:when test="$len = 0">
      <xsl:value-of select="$line"/>
      <xsl:text>&nl;</xsl:text>
    </xsl:when>
    <xsl:when test="$len mod 2 = 1">
      <xsl:call-template name="ruler">
	      <xsl:with-param name="char" select="concat($char,$char)"/>
	      <xsl:with-param name="len" select="($len - 1) div 2"/>
        <xsl:with-param name="line" select="concat($line,$char)"/>
	    </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
	    <xsl:call-template name="ruler">
	      <xsl:with-param name="char" select="concat($char,$char)"/>
	      <xsl:with-param name="len" select="$len div 2"/>
        <xsl:with-param name="line" select="$line"/>
	    </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


</xsl:stylesheet>

