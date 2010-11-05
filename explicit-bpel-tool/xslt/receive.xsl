<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable"
  xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"
  xmlns:plnk="http://docs.oasis-open.org/wsbpel/2.0/plnktype">

  <!-- Unfold receive -->
  <xsl:template match="bpel:receive">
    <xsl:param name="uniquePrefix" select="'v0'" />
    <xsl:variable name="variable" select="@variable" />
    <xsl:variable name="outputElement" select="count(ancestor::*/bpel:variables/bpel:variable[@name=$variable][1]/@element) = 1" />
    
    <xsl:choose>
      <xsl:when test="bpel:fromParts or $outputElement">
        <xsl:variable name="partnerLinkName" select="@partnerLink" />
        <xsl:variable name="operation" select="@operation" />
        <xsl:variable name="partnerLink" select="(ancestor::*/bpel:partnerLinks/bpel:partnerLink[@name=$partnerLinkName])[1]" />
        <xsl:variable name="partnerLinkNamespace" select="namespace::*[local-name() = substring-before($partnerLink/@partnerLinkType, ':')]" />
        <xsl:variable name="definitions" select="document(/bpel:process/bpel:import[@importType='http://schemas.xmlsoap.org/wsdl/']/@location)/wsdl:definitions[@targetNamespace=$partnerLinkNamespace]" />
        <xsl:variable name="portType" select="substring-after($definitions/plnk:partnerLinkType[@name=substring-after($partnerLink/@partnerLinkType, ':')]/plnk:role[@name=$partnerLink/@myRole]/@portType, ':')" />
        
        <bpel:scope>
          <xsl:message terminate="no">Implicit scope in receive made explicit.</xsl:message>
          <xsl:copy-of select="@name" />
          <xsl:copy-of select="@suppressJoinFailure" />
          <xsl:copy-of select="bpel:targets" />
          <xsl:copy-of select="bpel:sources" />
          
          <xsl:message terminate="no">Implicit temporary variables in receive made explicit.</xsl:message>
          
          <bpel:variables>
            <!-- add temporary output message variable -->
            <bpel:variable>
              <xsl:attribute name="name">
                <xsl:value-of select="concat($uniquePrefix, 'OutputMessage')" />
              </xsl:attribute>
            
              <xsl:attribute name="messageType">
                <xsl:variable name="message" select="$definitions/wsdl:portType[@name=$portType]/wsdl:operation[@name=$operation]/wsdl:input" />
                <xsl:variable name="messageNamespace" select="$message/namespace::*[local-name() = substring-before($message/@message, ':')]" />
                <xsl:variable name="namespacePrefix" select="name(namespace::*[self::node() = $messageNamespace][1])" />
                <xsl:value-of select="concat($namespacePrefix, ':', substring-after($message/@message, ':'))" />
              </xsl:attribute>
            </bpel:variable>
          </bpel:variables>
          
          <xsl:message terminate="no">Implicit assignments in receive made explicit.</xsl:message>
          <bpel:sequence>
            
            <xsl:message>Transforming a Receive activity '<xsl:value-of select="@name" />' into a Pick activity.</xsl:message>
            <bpel:pick>
              <xsl:copy-of select="@createInstance" />
              <xsl:copy-of select="@name" />
              <xsl:copy-of select="@suppressJoinFailure" />      
              <xsl:copy-of select="bpel:targets" />
              <xsl:copy-of select="bpel:sources" />
              <bpel:onMessage>
                <xsl:copy-of select="@partnerLink" />
                <xsl:copy-of select="@operation" />
                <xsl:attribute name="variable">
                  <xsl:value-of select="concat($uniquePrefix, 'OutputMessage')" />
                </xsl:attribute>
                <xsl:copy-of select="@messageExchange" />
                <xsl:copy-of select="bpel:correlations" />
                <bpel:empty/>      
              </bpel:onMessage>
            </bpel:pick>
            
            <!-- Transform fromParts into an assignment, if present -->
            <xsl:apply-templates select="bpel:fromParts" />
            
            <!-- Create assignment to copy single part to element variable -->
            <xsl:if test="$outputElement">
              <bpel:assign>
                <bpel:copy keepSrcElementName="yes">
                  <bpel:from>
                    <xsl:attribute name="variable">
                      <xsl:value-of select="concat($uniquePrefix, 'OutputMessage')" />
                    </xsl:attribute>
                    <xsl:variable name="message" select="substring-after($definitions/wsdl:portType[@name=$portType]/wsdl:operation[@name=$operation]/wsdl:input/@message, ':')" />
                    <xsl:attribute name="part">
                      <xsl:value-of select="$definitions/wsdl:message[@name=$message]/wsdl:part/@name" />
                    </xsl:attribute>
                  </bpel:from>
                  <bpel:to>
                    <xsl:attribute name="variable">
                      <xsl:value-of select="@variable" />
                    </xsl:attribute>
                  </bpel:to>
                </bpel:copy>
              </bpel:assign>
            </xsl:if>
          </bpel:sequence>
        </bpel:scope>
      </xsl:when>
      <xsl:otherwise>
        <!-- A core receive activity, leaving out portType, if there -->
        <xsl:message>Transforming a Receive activity '<xsl:value-of select="@name" />' into a Pick activity.</xsl:message>
        <bpel:pick>
          <xsl:copy-of select="@createInstance" />
          <xsl:copy-of select="@name" />
          <xsl:copy-of select="@suppressJoinFailure" />      
          <xsl:copy-of select="bpel:targets" />
          <xsl:copy-of select="bpel:sources" />
          <bpel:onMessage>
            <xsl:copy-of select="@partnerLink" />
            <xsl:copy-of select="@operation" />
            <xsl:copy-of select="@variable" />
            <xsl:copy-of select="@messageExchange" />
            <xsl:copy-of select="bpel:correlations" />
            <bpel:empty/>      
          </bpel:onMessage>
        </bpel:pick>        
      </xsl:otherwise>
    </xsl:choose>
    
  </xsl:template>
  
</xsl:stylesheet>