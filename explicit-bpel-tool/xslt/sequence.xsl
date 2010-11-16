<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Transform <sequence>s into <flow>s. -->
<xsl:stylesheet
  version="1.0"
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

  <!-- Match the children of <sequence>s and wrap them in <flow>s
       with appropriate links. -->
  <xsl:template match="*" mode="sequenceChild">
    <xsl:param name="suppressJoinFailure" />

    <bpel:flow>
      <xsl:attribute name="suppressJoinFailure">
        <xsl:value-of select="$suppressJoinFailure" />
      </xsl:attribute>
    
      <xsl:if test="position() &gt; 1">
        <bpel:targets>
          <bpel:target>
            <xsl:call-template name="attribute-with-unique-element-name">
              <xsl:with-param name="attributeName" select="'linkName'" />
              <xsl:with-param name="element" select="preceding-sibling::*[1]" />
              <xsl:with-param name="postfix" select="'SequenceLink'" />
            </xsl:call-template>
          </bpel:target>
        </bpel:targets>        
      </xsl:if>

      <xsl:if test="following-sibling::bpel:*">
        <bpel:sources>
          <bpel:source>
            <xsl:call-template name="attribute-with-unique-element-name">
              <xsl:with-param name="attributeName" select="'linkName'" />
              <xsl:with-param name="element" select="." />
              <xsl:with-param name="postfix" select="'SequenceLink'" />
            </xsl:call-template>
            <bpel:transitionCondition expressionLanguage="urn:oasis:names:tc:wsbpel:2.0:sublang:xpath1.0">
              true()
            </bpel:transitionCondition>
          </bpel:source>
        </bpel:sources>
      </xsl:if>
      
      <xsl:choose>
        <xsl:when test="self::bpel:sequence">
          <xsl:call-template name="sequence" />
        </xsl:when>
      
        <xsl:otherwise>
		      <xsl:copy>
		        <xsl:copy-of select="@*" />
		        <xsl:apply-templates select="*" />
		      </xsl:copy>
		    </xsl:otherwise>
      </xsl:choose>
      
    </bpel:flow>
  </xsl:template>

  <xsl:template match="bpel:sequence" name="sequence">
    <bpel:flow>
      <xsl:copy-of select="@*[not(namespace-uri() = '' and local-name() = 'suppressJoinFailure')]" />
      <xsl:attribute name="suppressJoinFailure">yes</xsl:attribute>
      
      <xsl:apply-templates select="bpel:targets | bpel:sources" />

      <xsl:choose>
        <xsl:when test="1 &lt; count(bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:extensionActivity | bpel:flow | bpel:forEach | bpel:if | bpel:invoke | bpel:pick | bpel:receive | bpel:repeatUntil | bpel:reply | bpel:rethrow | bpel:scope | bpel:sequence | bpel:throw | bpel:validate | bpel:wait | bpel:while)">
	        <xsl:call-template name="sequence-links" />
	        <xsl:apply-templates select="bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:extensionActivity | bpel:flow | bpel:forEach | bpel:if | bpel:invoke | bpel:pick | bpel:receive | bpel:repeatUntil | bpel:reply | bpel:rethrow | bpel:scope | bpel:sequence | bpel:throw | bpel:validate | bpel:wait | bpel:while"
	                             mode="sequenceChild">
	          <xsl:with-param name="suppressJoinFailure">
	            <xsl:value-of select="ancestor-or-self::*[@suppressJoinFailure][1]/@suppressJoinFailure" />
	          </xsl:with-param>
	        </xsl:apply-templates>
	      </xsl:when>
	
	      <xsl:otherwise>
          <xsl:apply-templates select="bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:extensionActivity | bpel:flow | bpel:forEach | bpel:if | bpel:invoke | bpel:pick | bpel:receive | bpel:repeatUntil | bpel:reply | bpel:rethrow | bpel:scope | bpel:sequence | bpel:throw | bpel:validate | bpel:wait | bpel:while" />
	      </xsl:otherwise>
      </xsl:choose>
      
      <xsl:apply-templates select="*[not(self::bpel:targets or self::bpel:sources or self::bpel:assign or self::bpel:compensate or self::bpel:compensateScope or self::bpel:empty or self::bpel:exit or self::bpel:extensionActivity or self::bpel:flow or self::bpel:forEach or self::bpel:if or self::bpel:invoke or self::bpel:pick or self::bpel:receive or self::bpel:repeatUntil or self::bpel:reply or self::bpel:rethrow or self::bpel:scope or self::bpel:sequence or self::bpel:throw or self::bpel:validate or self::bpel:wait or self::bpel:while)]" />

    </bpel:flow>
  </xsl:template>

  <xsl:template name="sequence-links">
    <bpel:links>
      <xsl:for-each select="bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:extensionActivity | bpel:flow | bpel:forEach | bpel:if | bpel:invoke | bpel:pick | bpel:receive | bpel:repeatUntil | bpel:reply | bpel:rethrow | bpel:scope | bpel:sequence | bpel:throw | bpel:validate | bpel:wait | bpel:while">
        <xsl:if test="following-sibling::bpel:*">
          <bpel:link>
            <xsl:call-template name="attribute-with-unique-element-name">
              <xsl:with-param name="attributeName" select="'name'" />
              <xsl:with-param name="element" select="." />
              <xsl:with-param name="postfix" select="'SequenceLink'" />
            </xsl:call-template>
          </bpel:link>
        </xsl:if>
      </xsl:for-each>
    </bpel:links>
  </xsl:template>

</xsl:stylesheet>