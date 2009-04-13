<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable"
  xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"
  xmlns:plnk="http://docs.oasis-open.org/wsbpel/2.0/plnktype">

  <!-- Unfold reply -->
  <xsl:template match="bpel:reply">
    <xsl:param name="uniquePrefix" select="'v0'" />
    <xsl:variable name="variable" select="@variable" />
    <xsl:variable name="inputElement" select="count(ancestor::*/bpel:variables/bpel:variable[@name=$variable][1]/@element) = 1" />
    
    <xsl:choose>
      <xsl:when test="bpel:toParts or $inputElement">
        <xsl:variable name="partnerLinkName" select="@partnerLink" />
        <xsl:variable name="operation" select="@operation" />
        <xsl:variable name="partnerLink" select="(ancestor::*/bpel:partnerLinks/bpel:partnerLink[@name=$partnerLinkName])[1]" />
        <xsl:variable name="partnerLinkNamespace" select="namespace::*[local-name() = substring-before($partnerLink/@partnerLinkType, ':')]" />
        <xsl:variable name="definitions" select="document(/bpel:process/bpel:import[@importType='http://schemas.xmlsoap.org/wsdl/']/@location)/wsdl:definitions[@targetNamespace=$partnerLinkNamespace]" />
        <xsl:variable name="portType" select="substring-after($definitions/plnk:partnerLinkType[@name=substring-after($partnerLink/@partnerLinkType, ':')]/plnk:role[@name=$partnerLink/@myRole]/@portType, ':')" />
        
        <scope>
          <xsl:message terminate="no">Implicit scope in reply made explicit.</xsl:message>
          <xsl:copy-of select="@name" />
          <xsl:copy-of select="@suppressJoinFailure" />
          <xsl:copy-of select="bpel:targets" />
          <xsl:copy-of select="bpel:sources" />
          
          <xsl:message terminate="no">Implicit temporary variables in reply made explicit.</xsl:message>
          
          <variables>
            <!-- add temporary output message variable -->
            <variable>
              <xsl:attribute name="name">
                <xsl:value-of select="concat($uniquePrefix, 'InputMessage')" />
              </xsl:attribute>
              <xsl:attribute name="messageType">
                <xsl:variable name="message" select="$definitions/wsdl:portType[@name=$portType]/wsdl:operation[@name=$operation]/wsdl:output" />
                <xsl:variable name="messageNamespace" select="$message/namespace::*[local-name() = substring-before($message/@message, ':')]" />
                <xsl:variable name="namespacePrefix" select="name(namespace::*[self::node() = $messageNamespace][1])" />
                <xsl:value-of select="concat($namespacePrefix, ':', substring-after($message/@message, ':'))" />
              </xsl:attribute>
            </variable>
          </variables>
          
          <xsl:message terminate="no">Implicit assignments in invoke made explicit.</xsl:message>
          <sequence>
            
            <!-- Transform toParts into an assignment, if present -->
            <xsl:apply-templates select="bpel:toParts" />
            
            <!-- Create assignment to copy element variable to single part -->
            <xsl:if test="$inputElement">
              <assign>
                <copy keepSrcElementName="yes">
                  <from>
                    <xsl:attribute name="variable">
                      <xsl:value-of select="@variable" />
                    </xsl:attribute>
                  </from>
                  <to>
                    <xsl:variable name="message" select="substring-after($definitions/wsdl:portType[@name=$portType]/wsdl:operation[@name=$operation]/wsdl:output/@message, ':')" />
                    <xsl:attribute name="variable">
                      <xsl:value-of select="concat($uniquePrefix, 'InputMessage')" />
                    </xsl:attribute>
                    <xsl:attribute name="part">
                      <xsl:value-of select="$definitions/wsdl:message[@name=$message]/wsdl:part/@name" />
                    </xsl:attribute>
                  </to>
                </copy>
              </assign>
            </xsl:if>
            
            <xsl:copy>
              <xsl:copy-of select="@name" />
              <xsl:copy-of select="@partnerLink" />
              <xsl:copy-of select="@faultName" />
              <xsl:copy-of select="@operation" />
              <xsl:copy-of select="@messageExchange" />
              <xsl:attribute name="variable">
                <xsl:value-of select="concat($uniquePrefix, 'InputMessage')" />
              </xsl:attribute>
              <xsl:copy-of select="bpel:correlations" />
            </xsl:copy>
            
          </sequence>
        </scope>
      </xsl:when>
      <xsl:otherwise>
        <!-- A core reply activity, leaving out portType, if there -->
        <xsl:copy>
          <xsl:copy-of select="@*[namespace-uri()='' and not(@portType)]" />
          <xsl:copy-of select="bpel:targets" />
          <xsl:copy-of select="bpel:sources" />
          <xsl:copy-of select="bpel:correlations" />
        </xsl:copy>
      </xsl:otherwise>
    </xsl:choose>
    
  </xsl:template>
  
</xsl:stylesheet>