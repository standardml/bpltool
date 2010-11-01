<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Add default <messageExchange> to <process>, the immediate <scope> of <onEvent>, and parallel <forEach> -->
<!-- Also, make the use of these default message exchanges explicit -->
<!-- FIXME: how should we generate fresh names? -->
<!-- FIXME: how should we identify IMA/<reply> pairs? -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">

  <xsl:output indent="yes" method="xml" />

  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
  <!-- Add default message exchanges to the relevant elements that already have an <messageExchanges> element -->
  <xsl:template match="bpel:process/bpel:messageExchanges | bpel:onEvent/bpel:scope/bpel:messageExchanges | bpel:forEach[@parallel='yes']/bpel:scope/bpel:messageExchanges">
    <bpel:messageExchanges>
      <bpel:messageExchange name="FIXME fresh name" />
      <xsl:copy-of select="bpel:messageExchange"/>
    </bpel:messageExchanges>
  </xsl:template>
  
  <!-- Add <messageExchanges> elements and default message exchanges to the relevant elements that does not already have an <messageExchanges> element -->
  <xsl:template match="bpel:process[not(bpel:messageExchanges)]">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:copy-of select="bpel:extensions" />
      <xsl:copy-of select="bpel:import" />
      <xsl:copy-of select="bpel:partnerLinks" />
      
      <bpel:messageExchanges>
        <bpel:messageExchange name="FIXME fresh name" />
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
        <bpel:messageExchange name="FIXME fresh name" />
      </bpel:messageExchanges>
      
      <xsl:apply-templates select="*[not(
        self::bpel:partnerLinks or
        self::bpel:messageExchanges
        )]" />
    </xsl:copy>
  </xsl:template>

  <!-- FIXME identify IMA/<reply> pairs using the default message exchanges and make them explicit -->
  
</xsl:stylesheet>