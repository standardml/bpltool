<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">

  <xsl:variable name="other" select="'bpel:targets bpel:sources bpel:variables bpel:variables bpel:partnerLinks bpel:messageExchanges bpel:correlationSets bpel:eventHandlers bpel:faultHandlers bpel:compensationHandler bpel:terminationHandler'" />

  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="bpel:faultHandler[not(bpel:catchAll)]">
    <xsl:copy>
      <xsl:apply-templates select="bpel:catch" />
      <bpel:catchAll>
        <bpel:sequence>
          <bpel:compensate />
          <bpel:rethrow />
        </bpel:sequence>
      </bpel:catchAll>
    </xsl:copy>
    <xsl:message terminate="no">Added catchAll to exsisting faultHandlers</xsl:message>
  </xsl:template>

  <xsl:template match="bpel:scope[not(bpel:faultHandlers) or not(bpel:compensationHandler) or not(bpel:terminationHandler)]">
      <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates select="bpel:targets" />
      <xsl:apply-templates select="bpel:sources" />
      <xsl:apply-templates select="bpel:partnerLinks" />
      <xsl:apply-templates select="bpel:messageExchanges" />
      <xsl:apply-templates select="bpel:variables" />
      <xsl:apply-templates select="bpel:correlationSets" />
      
      <xsl:apply-templates select="bpel:faultHandlers" />
      <xsl:if test="not(bpel:faultHandlers)">
        <bpel:faultHandlers>
          <bpel:catchAll>
            <bpel:sequence>
              <bpel:compensate />
              <bpel:rethrow />
            </bpel:sequence>
          </bpel:catchAll>
        </bpel:faultHandlers>
        <xsl:message terminate="no">Added default faultHandlers</xsl:message>
      </xsl:if>
      
      <xsl:apply-templates select="bpel:compensationHandler" />
      <xsl:if test="not(bpel:compensationHandler)">
        <bpel:compensationHandler>
          <bpel:compensate />
        </bpel:compensationHandler>
        <xsl:message terminate="no">Added default compensationHandler</xsl:message>
      </xsl:if>
            
      <xsl:apply-templates select="bpel:terminationHandler" />
      <xsl:if test="not(bpel:terminationHandler)">
          <bpel:terminationHandler>
            <bpel:compensate />
          </bpel:terminationHandler>
        <xsl:message terminate="no">Added default terminationHandler</xsl:message>
      </xsl:if>
      <xsl:apply-templates select="bpel:eventHandlers" />
      
      <xsl:apply-templates select="*[not(contains($other,name()))]" />
    
    </xsl:copy>   
  </xsl:template>

</xsl:stylesheet>