<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  xmlns:dmy="http://beepell.com/samples/dummy/schema" version="1.0">

  <xsl:output indent="yes" method="xml" />

  <xsl:template match="dmy:person">
    <dmy:employee>
      <xsl:copy-of select="dmy:firstname" />
      <xsl:copy-of select="dmy:lastname" />
    </dmy:employee>
  </xsl:template>
  
</xsl:stylesheet>
