<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Add fresh names to unnamed <flow>s and <scope>s. -->
<!-- FIXME: how should we generate fresh names? -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">
  
  <xsl:output indent="yes" method="xml" />
  
  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="bpel:flow | bpel:scope">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:if test="not(@name)">
        <xsl:attribute name="name">
          FIXME fresh name
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>