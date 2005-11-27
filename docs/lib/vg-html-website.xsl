<?xml version="1.0"?> <!-- -*- sgml -*- -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!-- special stylesheet to generate the docs to fit into the website -->
<!-- the only differences between this and vg-html-chunk.xsl are:    -->
<!-- (a) no css stylesheet is used;        -->
<!-- (b) no navigation header is used;     -->
<!-- (c) no html start/end tags are output -->


<!-- import the common styles -->
<xsl:import href="vg-html-common.xsl"/>


<!-- custom header for website documentation            -->
<!-- the original template inserts html+title+body tags -->
<!-- see http://docbook.sourceforge.net/release/xsl/current/html/chunk-common.xsl -->
<xsl:template name="chunk-element-content">
  <xsl:param name="prev"/>
  <xsl:param name="next"/>
  <xsl:param name="nav.context"/>
  <xsl:param name="content">
    <xsl:apply-imports/>
  </xsl:param>
  <xsl:copy-of select="$content"/>
  <xsl:call-template name="footer.navigation">
	  <xsl:with-param name="prev" select="$prev"/>
	  <xsl:with-param name="next" select="$next"/>
	  <xsl:with-param name="nav.context" select="$nav.context"/>
  </xsl:call-template>
</xsl:template>


</xsl:stylesheet>
