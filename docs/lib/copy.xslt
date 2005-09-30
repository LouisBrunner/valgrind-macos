<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- This file was copied with some adaptations from the examples
supplied with the XSLT Cookbook by Sal Mangano, (C) 2003 O'Reilly &
Associates, ISBN 0-596-00372-2. -->

<xsl:output method="xml"/>

<xsl:template match="/ | node() | @* | comment() | processing-instruction()">
  <xsl:copy>
    <xsl:apply-templates select="node() | @*"/>
  </xsl:copy>
</xsl:template>

</xsl:stylesheet>
