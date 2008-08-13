<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet
  xmlns:xsl = "http://www.w3.org/1999/XSL/Transform"
  xmlns:xsd = "http://www.w3.org/2001/XMLSchema"
  version   = "1.0">

  <xsl:output indent="yes" method="xml"/>
  <xsl:strip-space elements="*"/>

  <!-- Copy all schema elements and attributes -->
  <xsl:template match="xsd:*">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

  <!-- Change first annotation to state modification -->
  <xsl:template match="xsd:schema/xsd:annotation">
    <xsd:annotation>
      <xsd:documentation>
        Schema for Executable Process for WS-BPEL 2.0
        Modified by Tim Hallwyl, for internal usage. 
      </xsd:documentation>
    </xsd:annotation>
  </xsl:template>
  


  <!-- SIMPLIFICATION OF QUERIES AND EXPRESSIONS -->

  <!-- Add xpath simple type to schema -->
  <xsl:template match="xsd:schema">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates />
      <xsd:simpleType name="xpath">
        <xsd:restriction base="xsd:string" />
      </xsd:simpleType>
      <xsl:message terminate="no">Added xpath (simple type).</xsl:message>
    </xsl:copy>
  </xsl:template>

  <!-- Make expressions simple (and unextensible) -->
  <xsl:template match="xsd:complexType[@name='tExpression']">
      <xsd:complexType name="tExpression">
        <xsd:simpleContent>
          <xsd:extension base="xpath"/>
        </xsd:simpleContent>
      </xsd:complexType>
      <xsl:message terminate="no">Changed tExpression (complex type) into a simple content type based on bpel:xpath.</xsl:message>
  </xsl:template>
  
  <!-- Make queries simple (and unextensible) -->
  <xsl:template match="xsd:complexType[@name='tQuery']">
      <xsd:complexType name="tQuery">
        <xsd:simpleContent>
          <xsd:extension base="xpath"/>
        </xsd:simpleContent>
      </xsd:complexType>
      <xsl:message terminate="no">Changed tQuery (complex type) into a simple content type based on bpel:xpath.</xsl:message>
  </xsl:template>
  
  <!-- Change expression extensions -->
  <xsl:template match="xsd:complexType[@name='tCondition']">
      <xsd:complexType name="tCondition">
        <xsd:simpleContent>
          <xsd:extension base="tExpression"/>
        </xsd:simpleContent>
      </xsd:complexType>
      <xsl:message terminate="no">Changed tCondition (complex type) into a simple type based on tExpression.</xsl:message>      
  </xsl:template> 
  
  <xsl:template match="xsd:complexType[@name='tBoolean-expr']">
      <xsd:complexType name="tBoolean-expr">
        <xsd:simpleContent>
          <xsd:extension base="tExpression"/>
        </xsd:simpleContent>
      </xsd:complexType>
      <xsl:message terminate="no">Changed tBoolean-expr (complex type) into a simple type based on tExpression.</xsl:message>      
  </xsl:template>
     
  <xsl:template match="xsd:complexType[@name='tDuration-expr']">
      <xsd:complexType name="tDuration-expr">
        <xsd:simpleContent>
          <xsd:extension base="tExpression"/>
        </xsd:simpleContent>
      </xsd:complexType>
      <xsl:message terminate="no">Changed tDuration-expr (complex type) into a simple type based on tExpression.</xsl:message>      
  </xsl:template>
    
  <xsl:template match="xsd:complexType[@name='tDeadline-expr']">
      <xsd:complexType name="tDeadline-expr">
        <xsd:simpleContent>
          <xsd:extension base="tExpression"/>
        </xsd:simpleContent>
      </xsd:complexType>
      <xsl:message terminate="no">Changed tDeadline-expr (complex type) into a simple type based on tExpression.</xsl:message>      
  </xsl:template>    



  <!-- EXPRESSIONS AND EXTENSIONS IN TO AND FROM SPECS -->

  <xsl:template match="xsd:complexType[@name='tTo']/xsd:sequence/xsd:any">
    <xsl:message terminate="no">Removed xsd:any from tTo.</xsl:message>
  </xsl:template>

  <xsl:template match="xsd:complexType[@name='tFrom']/xsd:sequence/xsd:any">
      <xsl:message terminate="no">Removed xsd:any from tFrom.</xsl:message>
  </xsl:template>



  <!-- GENERAL EXTENSIONS -->
  
  <!-- Remove all occurrences of anyAttribute () -->
  <xsl:template match="xsd:anyAttribute">
      <xsl:message terminate="no">Removed an anyAttribute.</xsl:message>
  </xsl:template>  
  
  <!-- Remove other occurrences of @expressionLanguage, than in tExpression -->
  <xsl:template match="xsd:attribute[@name='expressionLanguage']">
      <xsl:message terminate="no">Removed expressionLanguage attribute.</xsl:message>
  </xsl:template>  
  
  <!-- Remove other occurrences of @queryLanguage, than in tQuery -->
  <xsl:template match="xsd:attribute[@name='queryLanguage']">
      <xsl:message terminate="no">Removed queryLanguage attribute.</xsl:message>
  </xsl:template>  
  
  <!-- Remove tExtensibleElements -->
  <xsl:template match="xsd:complexType[@name='tExtensibleElements']">
    <xsl:message terminate="no">Removed tExtensibleElements (complex type).</xsl:message>
  </xsl:template>

  <!-- Narrow exteded element with tExtensibleElements. -->
  <xsl:template match="xsd:complexContent[child::xsd:extension[@base = 'tExtensibleElements']]">
    <xsl:apply-templates select="child::xsd:extension/*"/> 
    <xsl:message terminate="no">Removed an tExtensibleElements base extension.</xsl:message>
  </xsl:template>



  <!-- DOCUMENTATION -->

  <!-- Remove all references to documentation -->
  <xsl:template match="xsd:element[@ref='documentation']">
    <xsl:message terminate="no">Removed documentation (element reference).</xsl:message>
  </xsl:template>
  
  <!-- Remove documentation element -->
  <xsl:template match="xsd:element[@name='documentation']">
    <xsl:message terminate="no">Removed documentation (element).</xsl:message>
  </xsl:template>
  
  <!-- Remove tDocumentation type -->
  <xsl:template match="xsd:complexType[@name='tDocumentation']">
    <xsl:message terminate="no">Removed tDocumentation (complex type).</xsl:message>
  </xsl:template>



  <!-- EXTENSION DECLARATIONS -->

  <!-- Remove extension element references -->
  <!-- (not expected, as it only occurs in tExtensions, which also is removed) -->
  <xsl:template match="xsd:element[@ref='extension']">
    <xsl:message terminate="no">Removed extension (element reference).</xsl:message>
  </xsl:template>

  <!-- Remove extension element -->
  <xsl:template match="xsd:element[@type='tExtension']">
    <xsl:message terminate="no">Removed extension (element).</xsl:message>
  </xsl:template>
  
  <!-- Remove extension complex type -->
  <xsl:template match="xsd:complexType[@name='tExtension']">
    <xsl:message terminate="no">Removed tExtension (complex type).</xsl:message>
  </xsl:template>



  <!-- EXTENSION ACTIVITY -->
  
  <!-- Remove extensionActivity element reference (in activity group) -->
  <xsl:template match="xsd:element[@ref='extensionActivity']">
    <xsl:message terminate="no">Removed extensionActivity (element reference).</xsl:message>
  </xsl:template>

  <!-- Remove extensionActivity element -->
  <xsl:template match="xsd:element[@type='tExtensionActivity']">
    <xsl:message terminate="no">Removed extensionActivity (element).</xsl:message>
  </xsl:template>
  
  <!-- Remove extensionActivity complex type -->
  <xsl:template match="xsd:complexType[@name='tExtensionActivity']">
    <xsl:message terminate="no">Removed tExtensionActivity (complex type).</xsl:message>
  </xsl:template>
  
  
  
  <!-- EXTENSION ASSIGN OPERATION -->  
  
  <!-- Remove extensionAssignOperation element reference (in assign) -->
  <xsl:template match="xsd:element[@ref='extensionAssignOperation']">
    <xsl:message terminate="no">Removed extensionAssignOperation (element reference).</xsl:message>
  </xsl:template>
  
  <!-- Remove extensionAssignOperation element -->
  <xsl:template match="xsd:element[@type='tExtensionAssignOperation']">
    <xsl:message terminate="no">Removed extensionAssignOperation (element).</xsl:message>
  </xsl:template>
  
  <!-- Remove extensionAssignOperation complex type -->
  <xsl:template match="xsd:complexType[@name='tExtensionAssignOperation']">
    <xsl:message terminate="no">Removed tExtensionAssignOperation (complex type).</xsl:message>
  </xsl:template>
  
  
  
  <!-- INVOKE, RECEIVE AND REPLY -->
    
  <!-- Remove error and compensation handlers, toParts and fromParts from invoke activity -->  
  <xsl:template match="xsd:complexType[@name='tInvoke']/xsd:complexContent/xsd:extension/xsd:sequence">
    <xsd:sequence>
      <xsl:copy-of select="xsd:element[(@ref!='catch' and @ref!='catchAll' and @ref!='compensationHandler' and @ref!='toParts' and @ref!='fromParts') or @name]"/>
    </xsd:sequence>
    <xsl:message terminate="no">Removed references to catch catchAll compensationHandler toParts fromParts and compensationHandler from tInvoke (complex type).</xsl:message>   
  </xsl:template>

  <!-- Remove references to fromParts from tReceive (complex type).  -->
  <xsl:template match="xsd:complexType[@name='tReceive']/xsd:complexContent/xsd:extension/xsd:sequence">
    <xsd:sequence>
      <xsl:copy-of select="xsd:element[(@ref!='fromParts') or @name]"/>
    </xsd:sequence>
    <xsl:message terminate="no">Removed references to toParts and fromParts from tReceive (complex type).</xsl:message>   
  </xsl:template>  
  
  <!-- Remove references to toParts from tReply (complex type). -->
  <xsl:template match="xsd:complexType[@name='tReply']/xsd:complexContent/xsd:extension/xsd:sequence">
    <xsd:sequence>
      <xsl:copy-of select="xsd:element[(@ref!='toParts') or @name]"/>
    </xsd:sequence>
    <xsl:message terminate="no">Removed references to toParts and fromParts from tReply (complex type).</xsl:message>   
  </xsl:template>  

  <!-- Removing portType attributes -->
  <xsl:template match="xsd:attribute[@name='portType']">
    <xsl:message terminate="no">Removed portType attribute.</xsl:message>   
  </xsl:template>  







  
</xsl:stylesheet>