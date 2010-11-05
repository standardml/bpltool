<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Make the default fault, compensation, and termination handlers explicit. -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">

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
            <!-- The below <flow> is a static desugaring of the following <sequence>:
            <bpel:sequence>
              <bpel:compensate />
              <bpel:rethrow />
            </bpel:sequence> -->          
            <bpel:flow>
              <bpel:links>
                <bpel:link name="l" />
              </bpel:links>
              <bpel:scope>
                <bpel:sources>
                  <bpel:source linkName="l">
                    <bpel:transitionCondition expressionLanguage="urn:oasis:names:tc:wsbpel:2.0:sublang:xpath1.0">
                      true()
                    </bpel:transitionCondition>
                  </bpel:source>
                </bpel:sources>
                <bpel:faultHandlers>
                  <bpel:catchAll>
                    <bpel:rethrow />
                  </bpel:catchAll>
                </bpel:faultHandlers>
				        <bpel:compensationHandler>
				          <bpel:empty />
				        </bpel:compensationHandler>
				        <bpel:terminationHandler>
				          <bpel:empty />
				        </bpel:terminationHandler>
                <bpel:compensate />
              </bpel:scope>
              <bpel:scope>
                <bpel:targets>
                  <bpel:target linkName="l">
                  </bpel:target>
                </bpel:targets>
                <bpel:faultHandlers>
                  <bpel:catchAll>
                    <bpel:rethrow />
                  </bpel:catchAll>
                </bpel:faultHandlers>
                <bpel:compensationHandler>
                  <bpel:empty />
                </bpel:compensationHandler>
                <bpel:terminationHandler>
                  <bpel:empty />
                </bpel:terminationHandler>
                <bpel:rethrow />
              </bpel:scope>
            </bpel:flow>
          </bpel:catchAll>
        </bpel:faultHandlers>
      </xsl:if>
      
      <xsl:apply-templates select="bpel:compensationHandler" />
      <xsl:if test="not(bpel:compensationHandler)">
        <bpel:compensationHandler>
          <bpel:compensate />
        </bpel:compensationHandler>
      </xsl:if>
            
      <xsl:apply-templates select="bpel:terminationHandler" />
      <xsl:if test="not(bpel:terminationHandler)">
        <bpel:terminationHandler>
          <bpel:compensate />
        </bpel:terminationHandler>
      </xsl:if>
    
      <xsl:apply-templates select="bpel:eventHandlers" />
      
      <xsl:apply-templates select="*[not(
        self::bpel:targets or 
        self::bpel:sources or 
        self::bpel:partnerLinks or 
        self::bpel:messageExchanges or 
        self::bpel:variables or 
        self::bpel:candrelationSets or 
        self::bpel:faultHandlers or 
        self::bpel:compensationHandler or 
        self::bpel:terminationHandler or 
        self::bpel:eventHandlers
      )]" />
    </xsl:copy>   
  </xsl:template>

</xsl:stylesheet>