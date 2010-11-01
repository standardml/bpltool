<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Make the simple default attribute values explicit.
     Attributes: createInstance, ignoreMissingFromData, initiate, isolated, succesfulBranchesOnly, validate -->

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
  
  <xsl:template match="bpel:pick | bpel:receive">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:if test="not(@createInstance)">
        <xsl:attribute name="createInstance">no</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="bpel:copy">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:if test="not(@ignoreMissingFromData)">
        <xsl:attribute name="ignoreMissingFromData">no</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="bpel:correlation">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:if test="not(@initiate)">
        <xsl:attribute name="initiate">no</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="bpel:scope">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:if test="not(@isolated)">
        <xsl:attribute name="isolated">no</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="bpel:branches">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:if test="not(@successfulBranchesOnly)">
        <xsl:attribute name="successfulBranchesOnly">no</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="bpel:assign">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:if test="not(@validate)">
        <xsl:attribute name="validate">no</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>