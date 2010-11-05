<?xml version="1.0" encoding="ISO-8859-1"?>

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

  <!-- Replace all extension activities with empty activities. -->
  <xsl:template match="bpel:extensionActivity">
    <bpel:empty>
      <xsl:copy-of select="child::*/@*[namespace-uri()='']" />
      <xsl:copy-of select="child::*/bpel:*" />
    </bpel:empty>
    <xsl:message terminate="no">Replaced an extensionActivity element with an empty activity.</xsl:message>
  </xsl:template>

</xsl:stylesheet>