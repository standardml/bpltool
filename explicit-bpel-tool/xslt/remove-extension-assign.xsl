<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Remove all <extensionAssignOperation>s and if this leads to an <assign>
     with no <copy> child elements, replace it with an <empty> element. -->

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
      <xsl:apply-templates select="*[not(self::bpel:extensionAssignOperation)]" />
    </bpel:empty>
  </xsl:template>

  <!-- Remove any remaining extensionAssignOperation elements -->
  <xsl:template match="bpel:extensionAssignOperation" />

</xsl:stylesheet>