<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">
  
  <xsl:output indent="yes" method="xml" />

  <xsl:param name="uniquePrefix" select="'v0'" />

  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="bpel:while">
    <bpel:if>
      <xsl:copy-of select="@*" />
      <xsl:copy-of select="bpel:targets" />
      <xsl:copy-of select="bpel:sources" />
      <bpel:condition><xsl:value-of select="bpel:condition/text()" /></bpel:condition>
      <bpel:repeatUntil>
        <xsl:apply-templates select="*[not(self::bpel:targets or self::bpel:sources or self::bpel:condition)]" />
        <bpel:condition>not(<xsl:value-of select="bpel:condition/text()" />)</bpel:condition>
      </bpel:repeatUntil>
    </bpel:if>
  </xsl:template>
  
</xsl:stylesheet>