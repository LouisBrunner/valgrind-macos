<?xml version="1.0"?> <!-- -*- sgml -*- -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!-- import the common styles -->
<xsl:import href="vg-html-common.xsl"/>


<!-- use our custom html stylesheet -->
<xsl:param name="html.stylesheet" select="'vg_basic.css'"/>


<!-- custom header for html documentation -->
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


</xsl:stylesheet>
