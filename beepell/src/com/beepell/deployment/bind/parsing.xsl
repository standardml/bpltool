<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">

  <xsl:output indent="yes" method="xml" />

  <!-- Copy all elements and attributes -->
  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="bpel:to[not(child::bpel:query) and not(child::bpel:literal) and child::text()]">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <expression><xsl:value-of select="self::node()" /></expression>
    </xsl:copy>
    <xsl:message terminate="no">Enclosed a 'to'-expression in tags</xsl:message>
  </xsl:template>
  
  <xsl:template match="bpel:from[not(child::bpel:query) and not(child::bpel:literal) and child::text()]">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <expression><xsl:value-of select="self::node()" /></expression>
    </xsl:copy>
    <xsl:message terminate="no">Enclosed 'from'-expression in tags, in a 'from'</xsl:message>
  </xsl:template>
  
        
</xsl:stylesheet>