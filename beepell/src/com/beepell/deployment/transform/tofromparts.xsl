<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable"
  xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"
  xmlns:plnk="http://docs.oasis-open.org/wsbpel/2.0/plnktype">

  <xsl:output indent="yes" method="xml" />

  <xsl:template match="bpel:toParts">
    <assign>
      <xsl:for-each select="bpel:toPart">
        <copy>
          <xsl:variable name="variableName" select="@fromVariable" />
          <xsl:if test="(ancestor::*/bpel:variables/bpel:variable[@name=$variableName and @element])[1]">
            <xsl:attribute name="keepSrcElementName">yes</xsl:attribute>
          </xsl:if>
          <from>
            <xsl:attribute name="variable">
              <xsl:value-of select="@fromVariable" />
            </xsl:attribute>
          </from>
          <to variable="temporaryInputMessage">
            <xsl:attribute name="part">
              <xsl:value-of select="@part" />
            </xsl:attribute>
          </to>
        </copy>
      </xsl:for-each>
    </assign>
  </xsl:template>

  <xsl:template match="bpel:fromParts">
    <assign>
      <xsl:for-each select="bpel:fromPart">
        <copy>
          <xsl:variable name="variableName" select="@toVariable" />
          <xsl:if test="(ancestor::*/bpel:variables/bpel:variable[@name=$variableName and @element])[1]">
            <xsl:attribute name="keepSrcElementName">yes</xsl:attribute>
          </xsl:if>
          <from variable="temporaryOutputMessage">
            <xsl:attribute name="part">
              <xsl:value-of select="@part" />
            </xsl:attribute>
          </from>
          <to>
            <xsl:attribute name="variable">
              <xsl:value-of select="@toVariable" />
            </xsl:attribute>
          </to>
        </copy>
      </xsl:for-each>
    </assign>
  </xsl:template>

</xsl:stylesheet>