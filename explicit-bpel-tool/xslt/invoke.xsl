<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Move the <scope>-parts of an <invoke> into an explicit enclosing <scope>. -->
<!-- Also, make temporary variables and assignments, due to the use of <toParts>,
     <fromParts>, and/or references to element variables, explicit. -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable"
  xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/"
  xmlns:plnk="http://docs.oasis-open.org/wsbpel/2.0/plnktype">

  <!-- Unfold invoke -->
  <xsl:template match="bpel:invoke">
    <xsl:param name="uniquePrefix" select="'v0'" />
    <xsl:variable name="inputVariable" select="@inputVariable" />
    <xsl:variable name="outputVariable" select="@outputVariable" />

    <xsl:variable name="inputElement" select="count(ancestor::*/bpel:variables/bpel:variable[@name=$inputVariable][1]/@element) = 1" />
    <xsl:variable name="outputElement" select="count(ancestor::*/bpel:variables/bpel:variable[@name=$outputVariable][1]/@element) = 1" />

    <xsl:choose>
      <xsl:when test="bpel:catch or bpel:catchAll or bpel:compensationHandler or bpel:toParts or bpel:fromParts or $inputElement or $outputElement">
        <xsl:variable name="partnerLinkName" select="@partnerLink" />
        <xsl:variable name="operation" select="@operation" />
        <xsl:variable name="partnerLink" select="(ancestor::*/bpel:partnerLinks/bpel:partnerLink[@name=$partnerLinkName])[1]" />
        <xsl:variable name="partnerLinkNamespace" select="namespace::*[local-name() = substring-before($partnerLink/@partnerLinkType, ':')]" />
        <xsl:variable name="definitions" select="document(/bpel:process/bpel:import[@importType='http://schemas.xmlsoap.org/wsdl/']/@location)/wsdl:definitions[@targetNamespace=$partnerLinkNamespace]" />
        <xsl:variable name="portType" select="substring-after($definitions/plnk:partnerLinkType[@name=substring-after($partnerLink/@partnerLinkType, ':')]/plnk:role[@name=$partnerLink/@partnerRole]/@portType, ':')" />

        <bpel:scope>
          <xsl:message terminate="no">Implicit scope in invoke made explicit.</xsl:message>
          <xsl:copy-of select="@name" />
          <xsl:copy-of select="@suppressJoinFailure" />
          <xsl:copy-of select="bpel:targets" />
          <xsl:copy-of select="bpel:sources" />

          <xsl:if test="bpel:toParts or bpel:fromParts or $inputElement or $outputElement">
            <xsl:message terminate="no">Implicit temporary variables in invoke made explicit.</xsl:message>

            <bpel:variables>
              <xsl:if test="bpel:toParts or $inputElement">
                <!-- add temporary input message variable -->
                <bpel:variable>
                  <xsl:attribute name="name">
                    <xsl:value-of select="concat($uniquePrefix, 'InputMessage')" />
                  </xsl:attribute>
                  <xsl:attribute name="messageType">
                    <xsl:variable name="message" select="$definitions/wsdl:portType[@name=$portType]/wsdl:operation[@name=$operation]/wsdl:input" />
                    <xsl:variable name="messageNamespace" select="$message/namespace::*[local-name() = substring-before($message/@message, ':')]" />
                    <xsl:variable name="namespacePrefix" select="name(namespace::*[self::node() = $messageNamespace][1])" />
                    <xsl:value-of select="concat($namespacePrefix, ':', substring-after($message/@message, ':'))" />
                  </xsl:attribute>
                </bpel:variable>
              </xsl:if>
              <xsl:if test="bpel:fromParts or $outputElement">
                <!-- add temporary output message variable -->
                <bpel:variable>
                  <xsl:attribute name="name">
                    <xsl:value-of select="concat($uniquePrefix, 'OutputMessage')" />
                  </xsl:attribute>
                  <xsl:attribute name="messageType">
                    <xsl:variable name="message" select="$definitions/wsdl:portType[@name=$portType]/wsdl:operation[@name=$operation]/wsdl:output" />
                    <xsl:variable name="messageNamespace" select="$message/namespace::*[local-name() = substring-before($message/@message, ':')]" />
                    <xsl:variable name="namespacePrefix" select="name(namespace::*[self::node() = $messageNamespace][1])" />
                    <xsl:value-of select="concat($namespacePrefix, ':', substring-after($message/@message, ':'))" />
                  </xsl:attribute>
                </bpel:variable>
              </xsl:if>
            </bpel:variables>
          </xsl:if>

          <!-- Move invokes local fault handlers to enclosing scope-->
          <xsl:if test="bpel:catch or bpel:catchAll">
            <!-- Wrap catch and/or catchAll in faultHandlers -->
            <bpel:faultHandlers>
              <xsl:copy-of select="bpel:catch" />
              <xsl:copy-of select="bpel:catchAll" />
            </bpel:faultHandlers>
          </xsl:if>

          <!-- Move invokes local compensation handler to enclosing scope-->
          <xsl:copy-of select="bpel:compensationHandler" />

          <xsl:choose>
            <xsl:when test="bpel:toParts or bpel:fromParts or $inputElement or $outputElement">
              <xsl:message terminate="no">Implicit assignments in invoke made explicit.</xsl:message>
              <bpel:sequence>
                <!-- Transform toParts into an assignment, if present -->
                <xsl:apply-templates select="bpel:toParts" />

                <!-- Create assignment to copy element variable to single part -->
                <xsl:if test="$inputElement">
                  <bpel:assign>
                    <bpel:copy keepSrcElementName="yes">
                      <bpel:from>
                        <xsl:attribute name="variable">
                          <xsl:value-of select="@inputVariable" />
                        </xsl:attribute>
                      </bpel:from>
                      <bpel:to>
                        <xsl:attribute name="variable">
                          <xsl:value-of select="concat($uniquePrefix, 'InputMessage')" />
                        </xsl:attribute>
                        <xsl:variable name="message" select="substring-after($definitions/wsdl:portType[@name=$portType]/wsdl:operation[@name=$operation]/wsdl:input/@message, ':')" />
                        <xsl:attribute name="part">
                          <xsl:value-of select="$definitions/wsdl:message[@name=$message]/wsdl:part/@name" />
                        </xsl:attribute>
                      </bpel:to>
                    </bpel:copy>
                  </bpel:assign>
                </xsl:if>

                <xsl:copy>
                  <xsl:copy-of select="@name" />
                  <xsl:copy-of select="@partnerLink" />
                  <xsl:copy-of select="@operation" />

                  <xsl:choose>
                    <xsl:when test="bpel:toParts or $inputElement">
                      <xsl:attribute name="inputVariable">
                        <xsl:value-of select="concat($uniquePrefix, 'InputMessage')" />
                      </xsl:attribute>
                    </xsl:when>
                    <xsl:otherwise>
                      <xsl:copy-of select="@inputVariable" />
                    </xsl:otherwise>
                  </xsl:choose>

                  <xsl:choose>
                    <xsl:when test="bpel:fromParts or $outputElement">
                      <xsl:attribute name="outputVariable">
                        <xsl:value-of select="concat($uniquePrefix, 'OutputMessage')" />
                      </xsl:attribute>
                    </xsl:when>
                    <xsl:otherwise>
                      <xsl:copy-of select="@outputVariable" />
                    </xsl:otherwise>
                  </xsl:choose>

                  <xsl:copy-of select="bpel:correlations" />
                </xsl:copy>

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
                        <xsl:variable name="message" select="substring-after($definitions/wsdl:portType[@name=$portType]/wsdl:operation[@name=$operation]/wsdl:output/@message, ':')" />
                        <xsl:attribute name="part">
                          <xsl:value-of select="$definitions/wsdl:message[@name=$message]/wsdl:part/@name" />
                        </xsl:attribute>
                      </bpel:from>
                      <bpel:to>
                        <xsl:attribute name="variable">
                          <xsl:value-of select="@outputVariable" />
                        </xsl:attribute>
                      </bpel:to>
                    </bpel:copy>
                  </bpel:assign>
                </xsl:if>
              </bpel:sequence>
            </xsl:when>
            <xsl:otherwise>
              <!-- Invoke without implict assignment, but with implict scope (due to error and compensation handlers) -->
              <xsl:copy>
                <xsl:copy-of select="@name" />
                <xsl:copy-of select="@partnerLink" />
                <xsl:copy-of select="@operation" />
                <xsl:copy-of select="@inputVariable" />
                <xsl:copy-of select="@outputVariable" />
                <xsl:copy-of select="bpel:correlations" />
              </xsl:copy>
            </xsl:otherwise>
          </xsl:choose>

        </bpel:scope>
      </xsl:when>
      <xsl:otherwise>
        <!-- A core invoke activity, leaving out portType, if there -->
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