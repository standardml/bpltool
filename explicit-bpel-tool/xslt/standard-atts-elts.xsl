<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Move <targets>, <sources>, name, and suppressJoinFailure from activities to a new wrapping <flow>. -->
<!-- Add fresh names to unnamed <flow>s and <scope>s. -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">

  <xsl:output indent="yes" method="xml" />

  <xsl:include href="fresh-names.xsl" />
    
  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
  <xsl:template name="name">
    <xsl:param name="name" />
    <xsl:choose>
      <xsl:when test="normalize-space(string($name)) = ''">
			  <xsl:call-template name="attribute-with-unique-element-name">
			    <xsl:with-param name="attributeName" select="'name'" />
          <xsl:with-param name="element" select="." />
          <xsl:with-param name="postfix" select="'FreshName'" />
			  </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="$name" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="*" mode="extensionActivity">
      <xsl:copy-of select="@*[not(namespace-uri() = '' and local-name() = 'suppressJoinFailure')]" />
      <xsl:copy-of select="*[not(self::bpel:targets or self::bpel:sources)]" />
  </xsl:template>

  <xsl:template match="bpel:extensionActivity[*[1]/bpel:targets or *[1]/bpel:sources or *[1]/@suppressJoinFailure]">
    <bpel:flow>
      <xsl:call-template name="name">
        <xsl:with-param name="name" select="*[1]/@name"/>
      </xsl:call-template>
      <xsl:copy-of select="*[1]/@suppressJoinFailure" />
      <xsl:copy-of select="*[1]/bpel:targets" />
      <xsl:copy-of select="*[1]/bpel:sources" />
      <xsl:copy>
        <xsl:apply-templates mode="extensionActivity" />
      </xsl:copy>
    </bpel:flow>
  </xsl:template>
  
  <xsl:template match="bpel:flow | bpel:scope">
    <xsl:copy>
	    <xsl:call-template name="name">
	      <xsl:with-param name="name" select="@name"/>
	    </xsl:call-template>
	    <xsl:copy-of select="@*[not(namespace-uri() = '' and local-name() = 'name')]" />
	    <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="bpel:scope[bpel:targets or bpel:sources]">
    <bpel:flow>
	    <xsl:call-template name="name">
	      <xsl:with-param name="name" select="@name"/>
	    </xsl:call-template>
      <xsl:copy-of select="@suppressJoinFailure" />
      <xsl:copy-of select="bpel:targets" />
      <xsl:copy-of select="bpel:sources" />
      <xsl:copy>
        <xsl:copy-of select="@*[not(namespace-uri() and local-name() = 'suppressJoinFailure')]" />
        <xsl:apply-templates select="*[not(self::bpel:targets or self::bpel:sources)]" />
      </xsl:copy>
    </bpel:flow>
  </xsl:template>

  <xsl:template match="bpel:receive[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:reply[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:invoke[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:assign[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:throw[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:exit[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:wait[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:empty[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:sequence[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:if[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:while[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:repeatUntil[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:forEach[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:pick[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:compensate[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:compensateScope[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:rethrow[bpel:targets or bpel:sources or @name or @suppressJoinFailure]
                     | bpel:validate[bpel:targets or bpel:sources or @name or @suppressJoinFailure]">
    <bpel:flow>
      <xsl:call-template name="name">
        <xsl:with-param name="name" select="@name"/>
      </xsl:call-template>
      <xsl:copy-of select="@suppressJoinFailure" />
      <xsl:copy-of select="bpel:targets" />
      <xsl:copy-of select="bpel:sources" />
      <xsl:copy>
        <xsl:copy-of select="@*[not(namespace-uri() = '' and
                                    (local-name() = 'name' or
                                     local-name() = 'suppressJoinFailure'))]" />
        <xsl:apply-templates select="*[not(self::bpel:targets or self::bpel:sources)]" />
      </xsl:copy>
    </bpel:flow>
  </xsl:template>
  
</xsl:stylesheet>