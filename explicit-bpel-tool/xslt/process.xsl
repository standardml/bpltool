<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Move the scope-parts of a <process> into an explicit <scope>. -->

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
      <xsl:copy-of select="@*" />
      <xsl:copy-of select="bpel:extensions" />
      <xsl:copy-of select="bpel:import" />
      <bpel:scope>
        <xsl:copy-of select="@*[not(namespace-uri() = '' and
                                    (local-name() = 'targetNamespace' or
                                     local-name() = 'expressionLanguage' or
                                     local-name() = 'queryLanguage'))]" />
        <xsl:apply-templates select="*[not(self::bpel:import or self::bpel:extensions)]" />
      </bpel:scope>
    </bpel:process>
  </xsl:template>
  
</xsl:stylesheet>