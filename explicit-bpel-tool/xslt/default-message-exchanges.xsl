<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Add default <messageExchange> to <process>, the immediate <scope> of <onEvent>, and parallel <forEach> -->
<!-- Also, make the use of these default message exchanges explicit -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">

  <xsl:output indent="yes" method="xml" />
  
  <xsl:include href="fresh-names.xsl"/>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>

  <!-- Construct the default message exchange name for the given scope
       (which defaults to the closest enclosing scope/process). -->
  <xsl:template name="default-message-exchange-name">
    <xsl:param name="scope" select="ancestor-or-self::*[self::bpel:process or self::bpel:scope[parent::bpel:onEvent or parent::bpel:forEach[@parallel='yes']]][1]" />
    <xsl:call-template name="unique-element-name">
      <xsl:with-param name="element" select="$scope" />
      <xsl:with-param name="postfix" select="'DefaultMessageExchange'" />
    </xsl:call-template>
  </xsl:template>
  
  <!-- Create the default message exchange for the given scope
       (which defaults to the closest enclosing scope/process). -->
  <xsl:template name="default-message-exchange">
    <xsl:param name="scope" select="ancestor-or-self::*[self::bpel:process or self::bpel:scope[parent::bpel:onEvent or parent::bpel:forEach[@parallel='yes']]][1]" />
    <bpel:messageExchange>
      <xsl:attribute name="name">
	      <xsl:call-template name="default-message-exchange-name">
	        <xsl:with-param name="scope" select="$scope" />
	      </xsl:call-template>
      </xsl:attribute>
    </bpel:messageExchange>
  </xsl:template>
  
  <!-- Add default message exchanges to the relevant elements that already have an <messageExchanges> element -->
  <xsl:template match="bpel:process/bpel:messageExchanges | bpel:onEvent/bpel:scope/bpel:messageExchanges | bpel:forEach[@parallel='yes']/bpel:scope/bpel:messageExchanges">
    <bpel:messageExchanges>
      <xsl:call-template name="default-message-exchange" />
      <xsl:copy-of select="bpel:messageExchange"/>
    </bpel:messageExchanges>
  </xsl:template>
  
  <!-- Add <messageExchanges> elements and default message exchanges to the
       relevant elements that does not already have an <messageExchanges> element -->
  <xsl:template match="bpel:process[not(bpel:messageExchanges)]">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:copy-of select="bpel:extensions" />
      <xsl:copy-of select="bpel:import" />
      <xsl:copy-of select="bpel:partnerLinks" />
      
      <bpel:messageExchanges>
        <xsl:call-template name="default-message-exchange" />
      </bpel:messageExchanges>
      
      <xsl:apply-templates select="*[not(
        self::bpel:extensions or
        self::bpel:import or
        self::bpel:partnerLinks or
        self::bpel:messageExchanges
        )]" />
    </xsl:copy>
  </xsl:template>
  <xsl:template match="bpel:onEvent/bpel:scope[not(bpel:messageExchanges)] | bpel:forEach[@parallel='yes']/bpel:scope[not(bpel:messageExchanges)]">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:copy-of select="bpel:partnerLinks" />
      
      <bpel:messageExchanges>
        <xsl:call-template name="default-message-exchange" />
      </bpel:messageExchanges>
      
      <xsl:apply-templates select="*[not(
        self::bpel:partnerLinks or
        self::bpel:messageExchanges
        )]" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="bpel:onMessage[not(@messageExchange)] | bpel:receive[not(@messageExchange)] | bpel:reply[not(@messageExchange)]">
    <xsl:copy>
      <xsl:attribute name="messageExchange">
        <xsl:call-template name="default-message-exchange-name" />
      </xsl:attribute>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
  <xsl:template match="bpel:onEvent[not(@messageExchange)]">
    <xsl:copy>
      <xsl:attribute name="messageExchange">
        <xsl:call-template name="default-message-exchange-name">
          <xsl:with-param name="scope" select="bpel:scope" />
        </xsl:call-template>
      </xsl:attribute>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>