<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Move <targets>, <sources>, and suppressJoinFailure from activities to a new wrapping <scope>. -->

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
  
  <xsl:template match="*" mode="extensionActivity">
      <xsl:copy-of select="@*[not(namespace-uri() = '' and local-name() = 'suppressJoinFailure')]" />
      <xsl:copy-of select="*[not(self::bpel:targets or self::bpel:sources)]" />
  </xsl:template>

  <xsl:template match="bpel:extensionActivity[*[1]/bpel:targets or *[1]/bpel:sources or *[1]/@bpel:suppressJoinFailure]">
    <bpel:scope>
      <xsl:copy-of select="*[1]/@bpel:suppressJoinFailure" />
      <xsl:copy-of select="*[1]/bpel:targets" />
      <xsl:copy-of select="*[1]/bpel:sources" />
      <xsl:copy>
        <xsl:apply-templates mode="extensionActivity" />
      </xsl:copy>
    </bpel:scope>
  </xsl:template>

  <xsl:template match="bpel:receive[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:reply[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:invoke[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:assign[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:throw[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:exit[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:wait[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:empty[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:sequence[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:if[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:while[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:repeatUntil[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:forEach[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:pick[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:flow[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:compensate[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:compensateScope[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:rethrow[bpel:targets or bpel:sources or @suppressJoinFailure]
                     | bpel:validate[bpel:targets or bpel:sources or @suppressJoinFailure]">
    <bpel:scope>
      <xsl:copy-of select="@suppressJoinFailure" />
      <xsl:copy-of select="bpel:targets" />
      <xsl:copy-of select="bpel:sources" />
      <xsl:copy>
        <xsl:copy-of select="@*[not(namespace-uri() = '' and
                                    local-name() = 'suppressJoinFailure')]" />
        <xsl:apply-templates select="*[not(self::bpel:targets or self::bpel:sources)]" />
      </xsl:copy>
    </bpel:scope>
  </xsl:template>
  
</xsl:stylesheet>