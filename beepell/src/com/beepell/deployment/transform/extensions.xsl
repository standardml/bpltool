<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable"
  xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"
  xmlns:plnk="http://docs.oasis-open.org/wsbpel/2.0/plnktype">

  <xsl:output indent="yes" method="xml" />
  

  <!-- Copy all elements and attributes in the bpel namespace -->
  <xsl:template match="bpel:*">
    <xsl:copy>
      <xsl:copy-of select="@*[namespace-uri()='']" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
  <!-- Leave out any extention elements (everything not matched by other templates) -->
  <xsl:template match="*">
    <xsl:message terminate="no">Removed an extension element</xsl:message>
  </xsl:template>
  
 
  <!-- Protect all literal values -->
  <xsl:template match="bpel:literal">
      <xsl:copy-of select="self::node()" />
  </xsl:template>
  
  <!-- Remove all ignorable extention declarations -->
  <xsl:template match="/bpel:process/bpel:extensions[count(child::bpel:extension[@mustUnderstand='yes']) = 0]">
    <xsl:message terminate="no">Removed extensions from process.</xsl:message>
  </xsl:template>

  <!-- Reject on mandatory extention declarations -->
  <xsl:template match="/bpel:process/bpel:extensions/bpel:extension[@mustUnderstand='yes']">
    <xsl:message terminate="yes">Mandatory extension is not supported.</xsl:message>
  </xsl:template>

  <!-- Replace any assign elements left without at least one copy child element -->
  <xsl:template match="bpel:assign[count(child::bpel:copy) = 0]">
    <empty>
      <xsl:copy-of select="@*[namespace-uri()='' and local-name() != 'validate']" />
      <xsl:copy-of select="./bpel:*[not(self::bpel:extensionAssignOperation)]" />
      <xsl:message terminate="no">Replaced an assign element with an empty activity.</xsl:message>
    </empty>
  </xsl:template>

  <!-- Remove any remaining extensionAssignOperation elements -->
  <xsl:template match="bpel:extensionAssignOperation">
    <xsl:message terminate="no">Removed extensionAssignOperation element.</xsl:message>
  </xsl:template>

  <!-- Remove any documentation elements -->
  <xsl:template match="bpel:documentation">
    <xsl:message terminate="no">Removed documentation element.</xsl:message>
  </xsl:template>

  <!-- Replace all extension activities with empty activities. -->
  <xsl:template match="bpel:extensionActivity">
    <empty>
      <xsl:copy-of select="child::*/@*[namespace-uri()='']" />
      <xsl:copy-of select="child::*/bpel:*" />
    </empty>
    <xsl:message terminate="no">Replaced an extensionActivity element with an empty activity.</xsl:message>
  </xsl:template>
  
  <!-- Remove any empty completion condition -->
  <xsl:template match="bpel:forEach/bpel:completionCondition[count(child::node()) = 0]">
    <xsl:message terminate="no">Removed an empty completion condition.</xsl:message>
  </xsl:template>

</xsl:stylesheet>