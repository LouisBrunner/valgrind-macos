<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="str.find-last"
  xmlns:str="http://www.ora.com/XSLTCookbook/namespaces/strings" extension-element-prefixes="str">

<!-- This file was copied with some adaptations from the examples
supplied with the XSLT Cookbook by Sal Mangano, (C) 2003 O'Reilly &
Associates, ISBN 0-596-00372-2. -->

<xsl:template name="str:substring-before-last"> 
  <xsl:param name="input"/>
  <xsl:param name="substr"/>
  
  <xsl:if test="$substr and contains($input, $substr)">
    <xsl:variable name="temp" select="substring-after($input, $substr)" />
    <xsl:value-of select="substring-before($input, $substr)" />
    <xsl:if test="contains($temp, $substr)">
      <xsl:value-of select="$substr" />
      <xsl:call-template name="str:substring-before-last">
        <xsl:with-param name="input" select="$temp" />
        <xsl:with-param name="substr" select="$substr" />
      </xsl:call-template>
    </xsl:if>
  </xsl:if>
  
</xsl:template>


<xsl:template name="str:substring-after-last">
  <xsl:param name="input"/>
  <xsl:param name="substr"/>
  
  <!-- Extract the string which comes after the first occurence -->
  <xsl:variable name="temp" select="substring-after($input,$substr)"/>
  
  <xsl:choose>
  	<xsl:when test="$substr and contains($temp,$substr)">
  		<xsl:call-template name="str:substring-after-last">
  			<xsl:with-param name="input" select="$temp"/>
  			<xsl:with-param name="substr" select="$substr"/>
  		</xsl:call-template>
  	</xsl:when>
  	<xsl:otherwise>
  		<xsl:value-of select="$temp"/>
  	</xsl:otherwise>
  </xsl:choose>
</xsl:template> 


<xsl:template match="xsl:stylesheet[@id='str.find-last'] | xsl:include[@href='str.find-last.xslt'] " >
<tests>

<!-- before -->
	<test name="str:substring-before-last with no occurences of yes">
	<xsl:call-template name="str:substring-before-last">
		<xsl:with-param name="input" select=" 'No occurences' "/>
		<xsl:with-param name="substr" select=" 'yes' "/>
	</xsl:call-template>
	</test>
	
	<test name="str:substring-before-last starts with yes">
	<xsl:call-template name="str:substring-before-last">
		<xsl:with-param name="input" select=" 'yes occurences' "/>
		<xsl:with-param name="substr" select=" 'yes' "/>
	</xsl:call-template>
	</test>

	<test name="str:substring-before-last starts with yes and ends with yes">
	<xsl:call-template name="str:substring-before-last">
		<xsl:with-param name="input" select=" 'yes occurences yes' "/>
		<xsl:with-param name="substr" select=" 'yes' "/>
	</xsl:call-template>
	</test>

	<test name="str:substring-before-last 3 yes">
	<xsl:call-template name="str:substring-before-last">
		<xsl:with-param name="input" select=" 'yesyesyes' "/>
		<xsl:with-param name="substr" select=" 'yes' "/>
	</xsl:call-template>
	</test>
	<test name="str:substring-before-last empty input">
	<xsl:call-template name="str:substring-before-last">
		<xsl:with-param name="substr" select=" 'yes' "/>
	</xsl:call-template>
	</test>
	
	<test name="str:substring-before-last empty search">
	<xsl:call-template name="str:substring-before-last">
		<xsl:with-param name="input" select=" 'No occurences' "/>
	</xsl:call-template>

	</test>

	<test name="str:substring-before-last large">
	<xsl:call-template name="str:substring-before-last">
		<xsl:with-param name="input" select=" 'yesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyesyes' "/>
		<xsl:with-param name="substr" select=" 'yes' "/>
	</xsl:call-template>


	</test>

<!-- after -->

	<test name="str:substring-after-last with no occurences of yes">
	<xsl:call-template name="str:substring-after-last">
		<xsl:with-param name="input" select=" 'No occurences' "/>
		<xsl:with-param name="substr" select=" 'yes' "/>
	</xsl:call-template>
	</test>
	
	<test name="str:substring-after-last starts with yes">
	<xsl:call-template name="str:substring-after-last">
		<xsl:with-param name="input" select=" 'yes occurences' "/>
		<xsl:with-param name="substr" select=" 'yes' "/>
	</xsl:call-template>
	</test>

	<test name="str:substring-after-last starts with yes and ends with yes">
	<xsl:call-template name="str:substring-after-last">
		<xsl:with-param name="input" select=" 'yes occurences yes' "/>
		<xsl:with-param name="substr" select=" 'yes' "/>
	</xsl:call-template>
	</test>

	<test name="str:substring-after-last 3 yes">
	<xsl:call-template name="str:substring-after-last">
		<xsl:with-param name="input" select=" 'yesyesyes' "/>
		<xsl:with-param name="substr" select=" 'yes' "/>
	</xsl:call-template>
	</test>

	<test name="str:substring-after-last 3 yes then no">
	<xsl:call-template name="str:substring-after-last">
		<xsl:with-param name="input" select=" 'yesyesyesno' "/>
		<xsl:with-param name="substr" select=" 'yes' "/>
	</xsl:call-template>
	</test>
	
	<test name="str:substring-after-last empty input">
	<xsl:call-template name="str:substring-after-last">
		<xsl:with-param name="substr" select=" 'yes' "/>
	</xsl:call-template>
	</test>
	
	<test name="str:substring-after-last empty search">
	<xsl:call-template name="str:substring-after-last">
		<xsl:with-param name="input" select=" 'No occurences' "/>
	</xsl:call-template>

	</test>

</tests>
</xsl:template>

 <xsl:template match="text()"/>
  
</xsl:stylesheet>
