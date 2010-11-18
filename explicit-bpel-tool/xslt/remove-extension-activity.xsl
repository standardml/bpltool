<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Replace all extension activities with empty activities. -->

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

  <xsl:template match="bpel:extensionActivity">
    <bpel:empty>
      <xsl:copy-of select="*/@*" />
      <xsl:apply-templates select="*/*" />
    </bpel:empty>
  </xsl:template>

</xsl:stylesheet>