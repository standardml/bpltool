<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Transform <sequence>s into <flow>s. -->
<xsl:stylesheet
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">

  <xsl:output indent="yes" method="xml" />

  <xsl:template match="*">
    <xsl:param name="uniquePrefix" select="'v0'" />

    <xsl:copy>
      <xsl:copy-of select="@*" />
      <xsl:apply-templates>
        <xsl:with-param name="uniquePrefix" select="string($uniquePrefix)" />
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>

  <!-- Match the children of <sequence>s and wrap them in <scope>s
       with appropriate links. -->
  <xsl:template match="*" mode="sequenceChild">
    <xsl:param name="uniquePrefix" />
    <xsl:param name="prefix" />

    <bpel:scope>
    
      <xsl:if test="position() &gt; 1">
        <bpel:targets>
          <bpel:target>
            <xsl:attribute name="linkName">
              <xsl:value-of select="$prefix" />
              <xsl:value-of select="position() - 1" />
            </xsl:attribute>
          </bpel:target>
        </bpel:targets>        
      </xsl:if>

      <xsl:if test="following-sibling::bpel:*">
        <bpel:sources>
          <bpel:source>
            <xsl:attribute name="linkName">
              <xsl:value-of select="$prefix" />
              <xsl:value-of select="position()" />
            </xsl:attribute>
            <bpel:transitionCondition expressionLanguage="urn:oasis:names:tc:wsbpel:2.0:sublang:xpath1.0">
              true()
            </bpel:transitionCondition>
          </bpel:source>
        </bpel:sources>
      </xsl:if>
      
      <xsl:choose>
        <xsl:when test="self::bpel:sequence">
          <xsl:call-template name="sequence">
            <xsl:with-param name="uniquePrefix" select="string($uniquePrefix)" />
          </xsl:call-template>
        </xsl:when>
      
        <xsl:otherwise>
		      <xsl:copy>
		        <xsl:copy-of select="@*" />
		        <xsl:apply-templates select="*">
		          <xsl:with-param name="uniquePrefix" select="string($uniquePrefix)" />
		        </xsl:apply-templates>
		      </xsl:copy>
		    </xsl:otherwise>
      </xsl:choose>
      
    </bpel:scope>
  </xsl:template>

  <xsl:template match="bpel:sequence" name="sequence">
    <xsl:param name="uniquePrefix" />
    <xsl:param name="childCount" select="count(bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:extensionActivity | bpel:flow | bpel:forEach | bpel:if | bpel:invoke | bpel:pick | bpel:receive | bpel:repeatUntil | bpel:reply | bpel:rethrow | bpel:scope | bpel:sequence | bpel:throw | bpel:validate | bpel:wait | bpel:while)" />
    
    <bpel:flow>
      <xsl:copy-of select="@*" />
      
      <xsl:apply-templates select="bpel:targets | bpel:sources" />

      <xsl:choose>
	      <xsl:when test="$childCount &gt; 1">
	        <xsl:variable name="childPrefix">
	          <xsl:value-of select="$uniquePrefix" />
	          <xsl:value-of select="string('s')" />
	          <xsl:number level="multiple" count="bpel:sequence" format="1.1"/>              
	          <xsl:value-of select="string('l')" />
	        </xsl:variable>
	      
	        <xsl:call-template name="sequence-links">
	          <xsl:with-param name="prefix" select="$childPrefix" />
	        </xsl:call-template>
	
	        <xsl:apply-templates select="bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:extensionActivity | bpel:flow | bpel:forEach | bpel:if | bpel:invoke | bpel:pick | bpel:receive | bpel:repeatUntil | bpel:reply | bpel:rethrow | bpel:scope | bpel:sequence | bpel:throw | bpel:validate | bpel:wait | bpel:while"
	                             mode="sequenceChild">
	          <xsl:with-param name="uniquePrefix" select="string($uniquePrefix)" />
	          <xsl:with-param name="prefix" select="$childPrefix" />
	        </xsl:apply-templates>
	      </xsl:when>
	
	      <!-- IF ONLY ONE CHILD IN THE SEQUENCE -->
	      <!-- THEN NO LINKS ARE NEEDED -->
	      <xsl:otherwise>
	        <xsl:apply-templates select="bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:extensionActivity | bpel:flow | bpel:forEach | bpel:if | bpel:invoke | bpel:pick | bpel:receive | bpel:repeatUntil | bpel:reply | bpel:rethrow | bpel:scope | bpel:sequence | bpel:throw | bpel:validate | bpel:wait | bpel:while">
	          <xsl:with-param name="uniquePrefix" select="string($uniquePrefix)" />
	        </xsl:apply-templates>
	      </xsl:otherwise>
      </xsl:choose>

    </bpel:flow>
  </xsl:template>

  <!-- creating numbered links using a prefix -->
  <xsl:template name="sequence-links">
    <xsl:param name="prefix" />

    <bpel:links>
      <xsl:for-each select="bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:extensionActivity | bpel:flow | bpel:forEach | bpel:if | bpel:invoke | bpel:pick | bpel:receive | bpel:repeatUntil | bpel:reply | bpel:rethrow | bpel:scope | bpel:sequence | bpel:throw | bpel:validate | bpel:wait | bpel:while">
        <xsl:if test="following-sibling::bpel:*">
          <bpel:link>
            <xsl:attribute name="name">
              <xsl:value-of select="$prefix" />
              <xsl:value-of select="position()" />
            </xsl:attribute>
          </bpel:link>
        </xsl:if>
      </xsl:for-each>
    </bpel:links>
  </xsl:template>

</xsl:stylesheet>