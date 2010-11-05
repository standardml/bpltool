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
  
  <!-- Replace any assign elements left without at least one copy child element -->
  <xsl:template match="bpel:assign[count(child::bpel:copy) = 0]">
    <bpel:empty>
      <xsl:copy-of select="@*[not(namespace-uri() = '' and local-name() = 'validate')]" />
      <xsl:copy-of select="./bpel:*[not(self::bpel:extensionAssignOperation)]" />
      <xsl:message terminate="no">Replaced an assign element with an empty activity.</xsl:message>
    </bpel:empty>
  </xsl:template>

  <!-- Remove any remaining extensionAssignOperation elements -->
  <xsl:template match="bpel:extensionAssignOperation">
    <xsl:message terminate="no">Removed extensionAssignOperation element.</xsl:message>
  </xsl:template>

</xsl:stylesheet>