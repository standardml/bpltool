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

  <xsl:template match="bpel:process">
    <xsl:copy>
      <xsl:copy-of select="@*" />
        <xsl:if test="not(@queryLanguage)">
          <xsl:attribute name="queryLanguage">urn:oasis:names:tc:wsbpel:2.0:sublang:xpath1.0</xsl:attribute>
        </xsl:if>
        <xsl:if test="not(@expressionLanguage)">
          <xsl:attribute name="expressionLanguage">urn:oasis:names:tc:wsbpel:2.0:sublang:xpath1.0</xsl:attribute>
        </xsl:if>
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>


  <xsl:template match="bpel:query">
    <xsl:copy>
      <xsl:copy-of select="@*" />
        <xsl:if test="not(@queryLanguage)">
          <xsl:attribute name="queryLanguage">urn:oasis:names:tc:wsbpel:2.0:sublang:xpath1.0</xsl:attribute>
         </xsl:if>
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>

  
  <xsl:template match="bpel:from | bpel:to | bpel:startCounterValue | bpel:finalCounterValue | bpel:branches | bpel:joinCondition | bpel:transitionCondition | bpel:condition | bpel:for | bpel:repeatEvery | bpel:until">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:if test="not(@expressionLanguage)">
        <xsl:attribute name="expressionLanguage">urn:oasis:names:tc:wsbpel:2.0:sublang:xpath1.0</xsl:attribute>
      </xsl:if>      
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>