<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Transform <sequence>s into <flow>s. -->
<xsl:stylesheet
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">

  <xsl:output indent="yes" method="xml" />

  <xsl:param name="uniquePrefix" select="'fresh'" />
  
  <xsl:template name="unique-element-name">
    <xsl:param name="element" />
    <xsl:param name="postfix" select="''" />
    <xsl:value-of select="$uniquePrefix" />
    <xsl:value-of select="generate-id($element)" />
    <xsl:value-of select="$postfix" />
  </xsl:template>
  
  <xsl:template name="attribute-with-unique-element-name">
    <xsl:param name="attributeName" />
    <xsl:param name="element" />
    <xsl:attribute name="{$attributeName}">
      <xsl:call-template name="unique-element-name">
        <xsl:with-param name="element" select="$element" />
      </xsl:call-template>
    </xsl:attribute>
  </xsl:template>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>

  <!-- Match the children of <sequence>s and wrap them in <flow>s
       with appropriate links. -->
  <xsl:template match="*" mode="sequenceChild">
    <bpel:flow>
    
      <xsl:if test="position() &gt; 1">
        <bpel:targets>
          <bpel:target>
            <xsl:call-template name="attribute-with-unique-element-name">
              <xsl:with-param name="attributeName" select="'linkName'" />
              <xsl:with-param name="element" select="preceding-sibling::*[1]" />
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
      <xsl:copy-of select="@*" />
      <xsl:apply-templates select="bpel:targets | bpel:sources" />

      <xsl:choose>
        <xsl:when test="1 &lt; count(bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:extensionActivity | bpel:flow | bpel:forEach | bpel:if | bpel:invoke | bpel:pick | bpel:receive | bpel:repeatUntil | bpel:reply | bpel:rethrow | bpel:scope | bpel:sequence | bpel:throw | bpel:validate | bpel:wait | bpel:while)">
	        <xsl:call-template name="sequence-links" />
	        <xsl:apply-templates select="bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:extensionActivity | bpel:flow | bpel:forEach | bpel:if | bpel:invoke | bpel:pick | bpel:receive | bpel:repeatUntil | bpel:reply | bpel:rethrow | bpel:scope | bpel:sequence | bpel:throw | bpel:validate | bpel:wait | bpel:while"
	                             mode="sequenceChild" />
	      </xsl:when>
	
	      <xsl:otherwise>
	        <xsl:apply-templates select="bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:extensionActivity | bpel:flow | bpel:forEach | bpel:if | bpel:invoke | bpel:pick | bpel:receive | bpel:repeatUntil | bpel:reply | bpel:rethrow | bpel:scope | bpel:sequence | bpel:throw | bpel:validate | bpel:wait | bpel:while" />
	      </xsl:otherwise>
      </xsl:choose>

    </bpel:flow>
  </xsl:template>

  <!-- creating numbered links using a prefix -->
  <xsl:template name="sequence-links">
    <bpel:links>
      <xsl:for-each select="bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:extensionActivity | bpel:flow | bpel:forEach | bpel:if | bpel:invoke | bpel:pick | bpel:receive | bpel:repeatUntil | bpel:reply | bpel:rethrow | bpel:scope | bpel:sequence | bpel:throw | bpel:validate | bpel:wait | bpel:while">
        <xsl:if test="following-sibling::bpel:*">
          <bpel:link>
            <xsl:call-template name="attribute-with-unique-element-name">
              <xsl:with-param name="attributeName" select="'name'" />
              <xsl:with-param name="element" select="." />
            </xsl:call-template>
          </bpel:link>
        </xsl:if>
      </xsl:for-each>
    </bpel:links>
  </xsl:template>

</xsl:stylesheet>