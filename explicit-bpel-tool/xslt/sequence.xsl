<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">

  <xsl:output indent="yes" method="xml" />

  <xsl:template match="*">
    <xsl:param name="uniquePrefix" select="string(v42)" />
    <xsl:param name="prefix" />
    <xsl:param name="isSqChild" />

    <xsl:if test="$isSqChild">
      <xsl:copy>
        <xsl:copy-of select="@*" />

        <xsl:call-template name="targets">
          <xsl:with-param name="isSqChild" select="$isSqChild" />
          <xsl:with-param name="prefix" select="$prefix" />
        </xsl:call-template>

        <xsl:call-template name="sources">
          <xsl:with-param name="isSqChild" select="$isSqChild" />
          <xsl:with-param name="prefix" select="$prefix" />
        </xsl:call-template>

        <xsl:apply-templates select="*[not(self::bpel:targets | self::bpel:sources)]">
          <xsl:with-param name="uniquePrefix" select="string($uniquePrefix)" />
        </xsl:apply-templates>
      </xsl:copy>
    </xsl:if>
    
    <xsl:if test="not($isSqChild)">
      <xsl:copy>
        <xsl:copy-of select="@*" />
        <xsl:apply-templates>
          <xsl:with-param name="uniquePrefix" select="string($uniquePrefix)" />
        </xsl:apply-templates>
      </xsl:copy>
    </xsl:if>
    
  </xsl:template>



  <xsl:template match="bpel:sequence">
    <xsl:param name="prefix" />
    <xsl:param name="isSqChild" />
    <xsl:param name="uniquePrefix" />
    <xsl:param name="childCount" select="count(bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:invoke | bpel:receive | bpel:reply | bpel:rethrow | bpel:throw | bpel:validate | bpel:wait | bpel:flow | bpel:forEach | bpel:if | bpel:pick | bpel:repeatUntil | bpel:sequence | bpel:while)" />
    <xsl:param name="target" />
    <xsl:param name="source" />
    <xsl:message>Transforming a Sequence activity '<xsl:value-of select="@name" />' with <xsl:value-of select="$childCount" /> child activities into a Flow activity.</xsl:message>
    
    <bpel:flow>
      <xsl:copy-of select="@*" />
        
      <xsl:call-template name="targets">
        <xsl:with-param name="isSqChild" select="$isSqChild" />
        <xsl:with-param name="prefix" select="$prefix" />
      </xsl:call-template>

      <xsl:call-template name="sources">
        <xsl:with-param name="isSqChild" select="$isSqChild" />
        <xsl:with-param name="prefix" select="$prefix" />
      </xsl:call-template>
        
        
        <xsl:if test="$childCount &gt; 1">
          <xsl:param name="childPrefix">
              <xsl:value-of select="$uniquePrefix" />
              <xsl:value-of select="string('s')" />
              <xsl:number level="multiple" count="bpel:sequence" format="1.1"/>              
              <xsl:value-of select="string('l')" />
          </xsl:param>
        
          <xsl:call-template name="sequence-links">
            <xsl:with-param name="prefix" select="$childPrefix" />
          </xsl:call-template>

          <xsl:apply-templates select="bpel:assign | bpel:compensate | bpel:compensateScope | bpel:empty | bpel:exit | bpel:invoke | bpel:receive | bpel:reply | bpel:rethrow | bpel:throw | bpel:validate | bpel:wait | bpel:flow | bpel:forEach | bpel:if | bpel:pick | bpel:repeatUntil | bpel:sequence | bpel:while">
            <xsl:with-param name="uniquePrefix" select="string($uniquePrefix)" />
            <xsl:with-param name="prefix" select="$childPrefix" />
            <xsl:with-param name="isSqChild" select="true()" />
          </xsl:apply-templates>
        </xsl:if>

        <!-- IF ONLY ONE CHILD IN THE SEQUENCE -->
        <!-- THEN NO LINKS ARE NEEDED -->
        <xsl:if test="$childCount = 1">
          <xsl:apply-templates>
            <xsl:with-param name="uniquePrefix" select="string($uniquePrefix)" />
          </xsl:apply-templates>
        </xsl:if>

    </bpel:flow>
  </xsl:template>


  <xsl:template name="targets">
    <xsl:param name="prefix" />
    <xsl:param name="isSqChild" />
    
    <xsl:if test="not($isSqChild) or position() = 1">
      <xsl:message>Copy targets from '<xsl:value-of select="@name"/>'.</xsl:message>
      <xsl:copy-of select="bpel:targets" />
    </xsl:if>

    <xsl:if test="$isSqChild and position() &gt; 1">
      <xsl:message>Creating targets in '<xsl:value-of select="@name"/>'.</xsl:message>
      <bpel:targets>

        <xsl:call-template name="join-condition">
          <xsl:with-param name="linkName">
            <xsl:value-of select="$prefix" />
            <xsl:value-of select="position() - 1" />
          </xsl:with-param>
        </xsl:call-template>

        <xsl:copy-of select="bpel:targets/bpel:target" />

        <xsl:call-template name="target" >
          <xsl:with-param name="prefix" select="$prefix" />
        </xsl:call-template>

      </bpel:targets>        
    </xsl:if>
  </xsl:template>

  <xsl:template name="sources">
    <xsl:param name="prefix" />
    <xsl:param name="isSqChild" />

    <xsl:if test="not($isSqChild) or not(following-sibling::bpel:*)">
      <xsl:copy-of select="bpel:sources" />
    </xsl:if>

    <xsl:if test="$isSqChild and following-sibling::bpel:*">
      <bpel:sources>
        <xsl:copy-of select="bpel:sources/bpel:source" />
        <xsl:call-template name="source">
          <xsl:with-param name="prefix" select="$prefix" />
        </xsl:call-template>
      </bpel:sources>
    </xsl:if>
  </xsl:template>

  <xsl:template name="target">
    <xsl:message>Adding TARGET for activity '<xsl:value-of select="@name"/>' at position <xsl:value-of select="position()"/></xsl:message>
    <xsl:param name="prefix" />
    <xsl:if test="position() &gt; 1">
      <bpel:target>
        <xsl:attribute name="linkName">
          <xsl:value-of select="$prefix" />
          <xsl:value-of select="position() - 1" />
        </xsl:attribute>
      </bpel:target>
    </xsl:if>
  </xsl:template>
  
  <xsl:template name="source">
    <xsl:message>Adding SOURCE for activity '<xsl:value-of select="@name"/>' at position <xsl:value-of select="position()"/></xsl:message>
    <xsl:param name="prefix" />
    <xsl:if test="following-sibling::bpel:*">
      <bpel:source>
        <xsl:attribute name="linkName">
          <xsl:value-of select="$prefix" />
          <xsl:value-of select="position()" />
        </xsl:attribute>
      </bpel:source>
    </xsl:if>
  </xsl:template>

  <!-- creating numbered links using a prefix -->
  <xsl:template name="sequence-links">
    <xsl:param name="prefix" />

    <bpel:links>
      <xsl:for-each select="bpel:*">
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

  <!-- Extend the join condition if it exist, otherwise do nothing -->
  <xsl:template name="join-condition">
    <xsl:param name="linkName" />
    <xsl:if test="bpel:targets/bpel:joinCondition">
      <bpel:joinCondition>(<xsl:value-of select="bpel:targets/bpel:joinCondition/text()" />) and $<xsl:value-of select="$linkName" /></bpel:joinCondition>
    </xsl:if>
  </xsl:template>


</xsl:stylesheet>