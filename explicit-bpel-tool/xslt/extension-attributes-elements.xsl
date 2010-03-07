<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">

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
  
</xsl:stylesheet>