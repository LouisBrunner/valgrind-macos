<?xml version="1.0" encoding="ISO-8859-1"?> <!-- -*- sgml -*- -->
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:ext="http://exslt.org/common"
                xmlns:str-split2lines-func="f:str-split2lines-func"
                exclude-result-prefixes="xsl ext str-split2lines-func">


<!-- how much do I love haskell ... -->
<xsl:template name="str-foldl">
  <xsl:param name="pFunc" select="/.."/>
  <xsl:param name="pA0"/>
  <xsl:param name="pStr"/>

  <xsl:choose>
    <xsl:when test="not(string($pStr))">
      <xsl:copy-of select="$pA0"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:variable name="vFunResult">
        <xsl:apply-templates select="$pFunc[1]">
          <xsl:with-param name="arg0" select="$pFunc[position() > 1]"/>
          <xsl:with-param name="arg1" select="$pA0"/>
          <xsl:with-param name="arg2" select="substring($pStr,1,1)"/>
        </xsl:apply-templates>
      </xsl:variable>

      <xsl:call-template name="str-foldl">
        <xsl:with-param name="pFunc" select="$pFunc"/>
        <xsl:with-param name="pStr" select="substring($pStr,2)"/>
        <xsl:with-param name="pA0" select="ext:node-set($vFunResult)"/>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>


<str-split2lines-func:str-split2lines-func/>

<xsl:template name="str-split-to-lines">
  <xsl:param name="pStr"/>
  <xsl:param name="pLineLength" select="60"/>
  <xsl:param name="pDelimiters" select="' &#xa;'"/>
  <xsl:variable name="vsplit2linesFun"
                select="document('')/*/str-split2lines-func:*[1]"/>

  <xsl:variable name="vrtfParams">
    <delimiters><xsl:value-of select="$pDelimiters"/></delimiters>
    <lineLength><xsl:copy-of select="$pLineLength"/></lineLength>
  </xsl:variable>

  <xsl:variable name="vResult">
    <xsl:call-template name="str-foldl">
      <xsl:with-param name="pFunc" select="$vsplit2linesFun"/>
      <xsl:with-param name="pStr" select="$pStr"/>

      <xsl:with-param name="pA0" select="ext:node-set($vrtfParams)"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:for-each select="ext:node-set($vResult)/line">
    <xsl:for-each select="word">
      <xsl:value-of select="concat(., ' ')"/>
    </xsl:for-each>
    <xsl:value-of select="'&#xa;'"/>
  </xsl:for-each>
</xsl:template>


<xsl:template match="str-split2lines-func:*">
  <xsl:param name="arg1" select="/.."/>
  <xsl:param name="arg2"/>

  <xsl:copy-of select="$arg1/*[position() &lt; 3]"/>
  <xsl:copy-of select="$arg1/line[position() != last()]"/>

  <xsl:choose>
    <xsl:when test="contains($arg1/*[1], $arg2)">
      <xsl:if test="string($arg1/word)">
        <xsl:call-template name="fillLine">
          <xsl:with-param name="pLine" select="$arg1/line[last()]"/>
          <xsl:with-param name="pWord" select="$arg1/word"/>
          <xsl:with-param name="pLineLength" select="$arg1/*[2]"/>
        </xsl:call-template>
      </xsl:if>
    </xsl:when>
    <xsl:otherwise>
      <xsl:copy-of select="$arg1/line[last()]"/>
      <word><xsl:value-of select="concat($arg1/word, $arg2)"/></word>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>
      

<!-- This template recognises every new word and accumulates the result,    -->
<!-- which is a list of 'line' elements, each having a list of 'word'       -->
<!-- children. After the last 'line' element there's a single 'word', in    -->
<!-- which the 'current word; is being accumulated.  Whenever the current   -->
<!-- character is one of the specified delimiters,  this signals the        -->
<!-- formation of a new word. This word is either added to the last line    -->
<!-- (if the total line length will not exceed the specified line-length),  -->
<!-- or a new line is started and this word becomes the 1st in the new line -->
<xsl:template name="fillLine">
  <xsl:param name="pLine" select="/.."/>
  <xsl:param name="pWord" select="/.."/>
  <xsl:param name="pLineLength" />

  <xsl:variable name="vnWordsInLine" select="count($pLine/word)"/>
  <xsl:variable name="vLineLength" 
                select="string-length($pLine) + $vnWordsInLine"/>

  <xsl:choose>
    <xsl:when test="not($vLineLength + string-length($pWord) > $pLineLength)">
      <line>
        <xsl:copy-of select="$pLine/*"/>
        <xsl:copy-of select="$pWord"/>
      </line>
    </xsl:when>
    <xsl:otherwise>
      <xsl:copy-of select="$pLine"/>
        <line>
          <xsl:copy-of select="$pWord"/>
        </line>
      <word/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>


</xsl:stylesheet>
