<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Remove names from all activities but scope. -->
<!-- Add fresh names to unnamed scopes. -->
<!-- FIXME: how should we generate fresh names? -->

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
  
  <!-- Remove names from all named activities except scopes. -->
  <xsl:template match="bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:invoke | bpel:receive | bpel:reply | bpel:rethrow | bpel:throw | bpel:validate | bpel:wait | bpel:flow | bpel:forEach | bpel:if | bpel:pick | bpel:repeatUntil | bpel:sequence | bpel:while">
    <xsl:copy>
      <xsl:copy-of select="@*[local-name() != 'name']"/>
      <xsl:apply-templates></xsl:apply-templates>
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="bpel:scope">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:if test="not(@name)">
        <xsl:attribute name="name">
          FIXME fresh name
        </xsl:attribute>
      </xsl:if>
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>