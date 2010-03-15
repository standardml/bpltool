<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet 
  version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable" 
  xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" 
  xmlns:plnk="http://docs.oasis-open.org/wsbpel/2.0/plnktype">

  <!-- ${workspace_loc:/core-bpel-tool/test/com/beepell/deployment/transform/pick-temp.bpel}  -->
  <xsl:output indent="yes" method="xml" />

  <xsl:param name="uniquePrefix">v0</xsl:param>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>

  <xsl:template name="variable">
    <xsl:variable name="partnerLinkName" select="@partnerLink" />
    <xsl:variable name="operation" select="@operation" />
    <xsl:variable name="partnerLink" select="(ancestor::*/bpel:partnerLinks/bpel:partnerLink[@name=$partnerLinkName])[1]" />
    <xsl:variable name="partnerLinkNamespace" select="namespace::*[local-name() = substring-before($partnerLink/@partnerLinkType, ':')]" />
    <xsl:variable name="definitions" select="document(/bpel:process/bpel:import[@importType='http://schemas.xmlsoap.org/wsdl/']/@location)/wsdl:definitions[@targetNamespace=$partnerLinkNamespace]" />
    <xsl:variable name="portType" select="substring-after($definitions/plnk:partnerLinkType[@name=substring-after($partnerLink/@partnerLinkType, ':')]/plnk:role[@name=$partnerLink/@myRole]/@portType, ':')" />
    <xsl:variable name="variable" select="@variable" />
    <xsl:variable name="outputElement" select="count(ancestor::*/bpel:variables/bpel:variable[@name=$variable][1]/@element) = 1" />

    <xsl:if test="bpel:fromParts or $outputElement">
      <variable>
        <xsl:attribute name="name">
          <xsl:value-of select="concat(concat($uniquePrefix, position()), 'OutputMessage')" />          
        </xsl:attribute>

        <xsl:attribute name="messageType">
          <xsl:variable name="message" select="$definitions/wsdl:portType[@name=$portType]/wsdl:operation[@name=$operation]/wsdl:input" />
          <xsl:variable name="messageNamespace" select="$message/namespace::*[local-name() = substring-before($message/@message, ':')]" />
          <xsl:variable name="namespacePrefix" select="name(namespace::*[self::node() = $messageNamespace][1])" />
          <xsl:value-of select="concat($namespacePrefix, ':', substring-after($message/@message, ':'))" />
        </xsl:attribute>
      </variable>
    </xsl:if>
  </xsl:template>
  
    <xsl:template name="onMessage">
    <xsl:param name="uniquePrefix" select="'v0'" />

    <xsl:variable name="partnerLinkName" select="@partnerLink" />
    <xsl:variable name="operation" select="@operation" />
    <xsl:variable name="partnerLink" select="(ancestor::*/bpel:partnerLinks/bpel:partnerLink[@name=$partnerLinkName])[1]" />
    <xsl:variable name="partnerLinkNamespace" select="namespace::*[local-name() = substring-before($partnerLink/@partnerLinkType, ':')]" />
    <xsl:variable name="definitions" select="document(/bpel:process/bpel:import[@importType='http://schemas.xmlsoap.org/wsdl/']/@location)/wsdl:definitions[@targetNamespace=$partnerLinkNamespace]" />
    <xsl:variable name="portType" select="substring-after($definitions/plnk:partnerLinkType[@name=substring-after($partnerLink/@partnerLinkType, ':')]/plnk:role[@name=$partnerLink/@myRole]/@portType, ':')" />
    <xsl:variable name="variable" select="@variable" />
    <xsl:variable name="outputElement" select="count(ancestor::*/bpel:variables/bpel:variable[@name=$variable][1]/@element) = 1" />

    <xsl:copy>
      <xsl:copy-of select="@partnerLink" />
      <xsl:copy-of select="@operation" />
      <xsl:copy-of select="@messageExchange" />
      <xsl:copy-of select="@variable" />
      <xsl:copy-of select="bpel:correlations" />
      
      <!-- if not core overwrite variable attribute and make assignments-->
      <xsl:if test="bpel:fromParts or $outputElement">
        <xsl:attribute name="variable">
            <xsl:value-of select="concat(concat($uniquePrefix, position()), 'OutputMessage')" />          
        </xsl:attribute>

        <xsl:message terminate="no">
          Implicit assignments in onMessage (pick) made explicit.
        </xsl:message>

        <sequence>
          <!-- Transform fromParts into an assignment, if present -->
          <xsl:apply-templates select="bpel:fromParts">
            <xsl:with-param name="uniquePrefix" select="concat($uniquePrefix, position())" />
          </xsl:apply-templates>

          <!-- Create assignment to copy single part to element variable -->
          <xsl:if test="$outputElement">
            <assign>
              <copy keepSrcElementName="yes">
                <from>
                  <xsl:attribute name="variable">
                  <xsl:value-of select="concat(concat($uniquePrefix, position()), 'OutputMessage')" />
                </xsl:attribute>
                  <xsl:variable name="message" select="substring-after($definitions/wsdl:portType[@name=$portType]/wsdl:operation[@name=$operation]/wsdl:input/@message, ':')" />
                  <xsl:attribute name="part">
                  <xsl:value-of select="$definitions/wsdl:message[@name=$message]/wsdl:part/@name" />
                </xsl:attribute>
                </from>
                <to>
                  <xsl:attribute name="variable">
                  <xsl:value-of select="@variable" />
                </xsl:attribute>
                </to>
              </copy>
            </assign>
          </xsl:if>

          <xsl:apply-templates select="bpel:*[not(local-name()='fromParts') and not(local-name()='correlations')]"/>

        </sequence>
      </xsl:if>
      
    </xsl:copy>
  </xsl:template>

  <xsl:template match="bpel:pick">
    <scope>
      <xsl:message terminate="no">
        Implicit scope in pick made explicit.
      </xsl:message>
      <xsl:copy-of select="@name" />
      <xsl:copy-of select="@suppressJoinFailure" />
      <xsl:copy-of select="bpel:targets" />
      <xsl:copy-of select="bpel:sources" />

      <variables>
        <xsl:message terminate="no">
          Implicit temporary variables in pick made explicit.
        </xsl:message>
        <xsl:for-each select="bpel:onMessage">
          <xsl:call-template name="variable" />
        </xsl:for-each>
      </variables>

      <xsl:copy>
        <xsl:copy-of select="@createInstance" />
        <xsl:copy-of select="@suppressJoinFailure" />
        <xsl:copy-of select="@name" />

        <xsl:for-each select="bpel:onMessage">
          <xsl:call-template name="onMessage" />
        </xsl:for-each>
        
        <xsl:apply-templates select="bpel:onAlarm" />
      </xsl:copy>
    </scope>

  </xsl:template>
  
  <xsl:template match="bpel:fromParts">
    <xsl:param name="uniquePrefix" select="'v0'" />
    <assign>
      <xsl:for-each select="bpel:fromPart">
        <copy>
          <xsl:variable name="variableName" select="@toVariable" />
          <xsl:if test="(ancestor::*/bpel:variables/bpel:variable[@name=$variableName and @element])[1]">
            <xsl:attribute name="keepSrcElementName">yes</xsl:attribute>
          </xsl:if>
          <from>
            <xsl:attribute name="variable">
              <xsl:value-of select="concat($uniquePrefix, 'OutputMessage')" />
            </xsl:attribute>
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