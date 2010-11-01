<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Remove the queryLanguage, expressionLanguage, suppressJoinFailure, and
     exitOnStandardFault attributes that have been made superfluous by making
     default values explicit. -->

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
  
  <xsl:template match="bpel:process">
    <bpel:process>
      <xsl:copy-of select="@*[local-name() != 'queryLanguage' and local-name() != 'expressionLanguage' and local-name() != 'suppressJoinFailure' and local-name() != 'exitOnStandardFault']" />
      <xsl:apply-templates />
    </bpel:process>
  </xsl:template>
  
</xsl:stylesheet>