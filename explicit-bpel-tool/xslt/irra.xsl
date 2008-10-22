<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output indent="yes" method="xml" />
  
  <!-- Copy all elements and attributes -->
  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
  <xsl:include href="invoke.xsl"/>
  <xsl:include href="receive.xsl"/>
  <xsl:include href="reply.xsl"/>
  <xsl:include href="tofromparts.xsl"/>
  
</xsl:stylesheet>