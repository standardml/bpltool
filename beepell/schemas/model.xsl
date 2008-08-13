<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet
  xmlns:xsl = "http://www.w3.org/1999/XSL/Transform"
  xmlns:xsd = "http://www.w3.org/2001/XMLSchema"
  version   = "1.0">

  <xsl:output indent="yes" method="xml"/>
  <xsl:strip-space elements="*"/>

  <!-- Copy all schema elements and attributes -->
  <xsl:template match="xsd:*">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>
  
  <!-- Add expression element to tFrom and remove mixed attribute --> 
  <xsl:template match="xsd:complexType[@name='tFrom']">
      <xsl:copy>
        <xsl:copy-of select="@*[local-name() != 'mixed']"/>
        <xsl:message terminate="no">Removed mixed attribute from tFrom.</xsl:message>
        <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="xsd:complexType[@name='tFrom']/xsd:sequence/xsd:choice">
      <xsl:copy>
        <xsl:copy-of select="@*"/>        
        <xsd:element name="expression" minOccurs="0" type="tExpression"/>
        <xsl:message terminate="no">Added xsd:element 'expression' in tFrom.</xsl:message>
        <xsl:apply-templates/>
      </xsl:copy>
  </xsl:template>
   
  <!-- Add expression element to tTo and remove mixed attribute --> 
  <xsl:template match="xsd:complexType[@name='tTo']">
      <xsl:copy>
        <xsl:copy-of select="@*[local-name() != 'mixed']"/>
        <xsl:message terminate="no">Removed mixed attribute from tTo.</xsl:message>
        <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="xsd:complexType[@name='tTo']/xsd:sequence">
    <xsl:copy>
    <xsl:copy-of select="@*"/>
      <xsd:choice minOccurs="0">
        <xsd:element name="expression" minOccurs="0" type="tExpression"/>
        <xsl:message terminate="no">Added xsd:element 'expression' in tFrom.</xsl:message>
        <xsl:apply-templates/>
      </xsd:choice>
    </xsl:copy>
  </xsl:template>
  
  
</xsl:stylesheet>