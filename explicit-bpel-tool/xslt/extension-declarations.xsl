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
  
  <!-- Remove all ignorable extention declarations -->
  <xsl:template match="/bpel:process/bpel:extensions[count(child::bpel:extension[@mustUnderstand='yes']) = 0]">
    <xsl:message terminate="no">Removed extensions from process.</xsl:message>
  </xsl:template>

  <!-- Reject on mandatory extention declarations -->
  <xsl:template match="/bpel:process/bpel:extensions/bpel:extension[@mustUnderstand='yes']">
    <xsl:message terminate="yes">Mandatory extension is not supported.</xsl:message>
  </xsl:template>

</xsl:stylesheet>