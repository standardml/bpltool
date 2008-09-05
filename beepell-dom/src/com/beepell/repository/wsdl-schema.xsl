<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet
  xmlns:xsl = "http://www.w3.org/1999/XSL/Transform"
  xmlns:wsdl = "http://schemas.xmlsoap.org/wsdl/"
  xmlns:xsd = "http://www.w3.org/2001/XMLSchema"
  version="1.0">
  
  <xsl:output indent="yes" method="xml" />
   
  <xsl:template match="/">
      <xsl:apply-templates select="/wsdl:definitions/wsdl:types/xsd:schema" />
  </xsl:template>
 
  <xsl:template match="xsd:schema">
      <xsl:message terminate="no">Found an XML Schema within the WSDL definition.</xsl:message>
      <xsl:copy-of select="self::node()" />
  </xsl:template>
  
</xsl:stylesheet>