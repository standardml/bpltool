<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">

  <xsl:output indent="yes" method="xml" />

  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="bpel:receive">
    <xsl:message>Transforming a Receive activity '<xsl:value-of select="@name" />' into a Pick activity.</xsl:message>
    <bpel:pick>
      <xsl:copy-of select="@createInstance" />
      <xsl:copy-of select="@name" />
      <xsl:copy-of select="@suppressJoinFailure" />      
      <xsl:copy-of select="bpel:targets" />
      <xsl:copy-of select="bpel:sources" />
      <bpel:onMessage>
        <xsl:copy-of select="@partnerLink" />
        <xsl:copy-of select="@operation" />
        <xsl:copy-of select="@variable" />
        <xsl:copy-of select="@messageExchange" />
        <xsl:copy-of select="bpel:correlations" />
        <bpel:empty/>      
      </bpel:onMessage>
    </bpel:pick>
  </xsl:template>

</xsl:stylesheet>