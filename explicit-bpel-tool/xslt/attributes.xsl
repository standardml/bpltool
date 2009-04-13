<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">
  
  <xsl:output indent="yes" method="xml" />

  <xsl:template match="bpel:process | bpel:scope">
    
    <xsl:param name="suppressJoinFailure">no</xsl:param>
    <xsl:param name="exitOnStandardFault">no</xsl:param>
    <xsl:copy>
      <xsl:copy-of select="@*" />
      
        <xsl:if test="not(@suppressJoinFailure)">
          <xsl:attribute name="suppressJoinFailure">
              <xsl:value-of select="$suppressJoinFailure" />
          </xsl:attribute>
        </xsl:if>
        
        <xsl:if test="not(@exitOnStandardFault)">
          <xsl:attribute name="exitOnStandardFault">
              <xsl:value-of select="$exitOnStandardFault" />
          </xsl:attribute>
        </xsl:if>
      
        <xsl:apply-templates>
        
          <xsl:if test="not(@suppressJoinFailure)">
            <xsl:with-param name="suppressJoinFailure" select="$suppressJoinFailure" />
          </xsl:if>

          <xsl:if test="not(@exitOnStandardFault)">
            <xsl:with-param name="exitOnStandardFault" select="$exitOnStandardFault" />
          </xsl:if>
          
          <xsl:if test="@suppressJoinFailure">
            <xsl:with-param name="suppressJoinFailure" select="@suppressJoinFailure" />
          </xsl:if>

          <xsl:if test="@exitOnStandardFault">          
              <xsl:with-param name="exitOnStandardFault" select="@exitOnStandardFault" />
          </xsl:if>
          
        </xsl:apply-templates>      
      </xsl:copy>
  </xsl:template>

  <xsl:template match="bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:invoke | bpel:receive | bpel:reply | bpel:rethrow | bpel:throw | bpel:validate | bpel:wait | bpel:flow | bpel:forEach | bpel:if | bpel:pick | bpel:repeatUntil | bpel:sequence | bpel:while">
  
    <xsl:param name="suppressJoinFailure">no</xsl:param>
    <xsl:param name="exitOnStandardFault">no</xsl:param>
    <xsl:copy>
      <xsl:copy-of select="@*" />      
        <xsl:if test="not(@suppressJoinFailure)">
          <xsl:attribute name="suppressJoinFailure">
              <xsl:value-of select="$suppressJoinFailure" />
          </xsl:attribute>
        </xsl:if>

        <xsl:apply-templates>
          <xsl:if test="not(@suppressJoinFailure)">
            <xsl:with-param name="suppressJoinFailure" select="$suppressJoinFailure" />
            <xsl:with-param name="exitOnStandardFault" select="$exitOnStandardFault" />
          </xsl:if>
          
          <xsl:if test="@suppressJoinFailure">
            <xsl:with-param name="suppressJoinFailure" select="@suppressJoinFailure" />
            <xsl:with-param name="exitOnStandardFault" select="$exitOnStandardFault" />
          </xsl:if>
        </xsl:apply-templates>
    
    </xsl:copy>
    
  </xsl:template>
  
  
  
  <xsl:template match="*">
    <xsl:param name="suppressJoinFailure">no</xsl:param>
    <xsl:param name="exitOnStandardFault">no</xsl:param>
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates>
        <xsl:with-param name="suppressJoinFailure" select="$suppressJoinFailure" />
        <xsl:with-param name="exitOnStandardFault" select="$exitOnStandardFault" />
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>