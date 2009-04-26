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
  
  <xsl:template match="bpel:repeatUntil">
    <bpel:sequence>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates select="*[not(self::bpel:condition)]" />
      <bpel:while>
        <bpel:condition>not(<xsl:value-of select="bpel:condition/text()" />)</bpel:condition>
        <xsl:apply-templates select="*[not(self::bpel:condition or self::bpel:sources or self::bpel:targets)]" />
      </bpel:while>
    </bpel:sequence>
  </xsl:template>
  
</xsl:stylesheet>