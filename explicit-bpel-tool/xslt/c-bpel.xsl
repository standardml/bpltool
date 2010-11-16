<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- FIXME this is outdated! -->

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
        This is the schema for CoreBPEL, a subset of WS-BPEL. 
        This Schema is a modified version of the original WS-BPEL 
        XML Schema, excluding implicit activities, extensions 
        and making default values mandatory.  
      </xsd:documentation>
    </xsd:annotation>
  </xsl:template>
  
  <!-- REMOVE SCOPE STUFF FROM PROCESS -->
  <xsl:template match="xsd:complexType[@name='tProcess']//xsd:element[@ref='partnerLinks']" />
  <xsl:template match="xsd:complexType[@name='tProcess']//xsd:element[@ref='messageExchanges']" />
  <xsl:template match="xsd:complexType[@name='tProcess']//xsd:element[@ref='variables']" />
  <xsl:template match="xsd:complexType[@name='tProcess']//xsd:element[@ref='correlationSets']" />
  <xsl:template match="xsd:complexType[@name='tProcess']//xsd:element[@ref='faultHandlers']" />
  <xsl:template match="xsd:complexType[@name='tProcess']//xsd:element[@ref='eventHandlers']" />
  <xsl:template match="xsd:complexType[@name='tProcess']//xsd:group[@ref='activity']">
    <xsd:element ref="scope" minOccurs="1" />
  </xsl:template>


  <!-- SIMPLIFICATION OF QUERIES AND EXPRESSIONS -->

  <!-- Add xpath simple type to schema -->
  <xsl:template match="xsd:schema">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates />
      <xsd:simpleType name="xpath">
        <xsd:restriction base="xsd:normalizedString" />
      </xsd:simpleType>
      <xsl:message terminate="no">Added xpath (simple type).</xsl:message>
    </xsl:copy>
  </xsl:template>

  <!-- Make expressions simple (and unextensible) -->
  <xsl:template match="xsd:complexType[@name='tExpression']">
      <xsd:complexType name="tExpression">
        <xsd:simpleContent>
          <xsd:extension base="xpath">
            <xsd:attribute name="expressionLanguage" type="xsd:anyURI" fixed="urn:oasis:names:tc:wsbpel:2.0:sublang:xpath1.0"/>    
          </xsd:extension>
        </xsd:simpleContent>
      </xsd:complexType>
      <xsl:message terminate="no">Changed tExpression (complex type) into a simple content type based on bpel:xpath.</xsl:message>
  </xsl:template>
  
  <!-- Make queries simple (and unextensible) -->
  <xsl:template match="xsd:complexType[@name='tQuery']">
      <xsd:complexType name="tQuery">
        <xsd:simpleContent>
          <xsd:extension base="xpath">
            <xsd:attribute name="queryLanguage" type="xsd:anyURI" fixed="urn:oasis:names:tc:wsbpel:2.0:sublang:xpath1.0"/>
          </xsd:extension>
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
  
  <!-- Remove tExtensibleElements -->
  <xsl:template match="xsd:complexType[@name='tExtensibleElements']">
    <xsl:message terminate="no">Removed tExtensibleElements (complex type).</xsl:message>
  </xsl:template>

  <!-- Narrow elements extended with tExtensibleElements. -->
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

  <!-- Remove extension element references (these are actually removed when removing tExpressions) -->
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

  <!-- Remove extensions element references -->
  <xsl:template match="xsd:element[@ref='extensions']">
    <xsl:message terminate="no">Removed extensions (element reference).</xsl:message>
  </xsl:template>

  <!-- Remove extensions element -->
  <xsl:template match="xsd:element[@type='tExtensions']">
    <xsl:message terminate="no">Removed extensions (element).</xsl:message>
  </xsl:template>
  
  <!-- Remove extensions complex type -->
  <xsl:template match="xsd:complexType[@name='tExtensions']">
    <xsl:message terminate="no">Removed tExtensions (complex type).</xsl:message>
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

  <!-- REMOVING RECEIVE (IT IS TRANSFORMED INTO A PICK ACTIVITY -->
  <xsl:template match="xsd:element[@ref='receive']" />
  <xsl:template match="xsd:element[@name='receive']" />
  <xsl:template match="xsd:complexType[@name='tReceive']" />

  <!-- REMOVE VARIABLE INITIALIZATION -->

  <xsl:template match="xsd:complexType[@name='tVariable']">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:copy-of select="xsd:complexContent/xsd:extension/xsd:attribute"/>
    </xsl:copy>
  </xsl:template>  

  <!-- REMOVE ELSEIF FROM IF -->
  <xsl:template match="xsd:element[@ref='elseif']" />
  <xsl:template match="xsd:element[@name='elseif']" />
  <xsl:template match="xsd:complexType[@name='tElseif']" />

  <!-- REMOVE WHILE -->
  <xsl:template match="xsd:element[@ref='while']" />
  <xsl:template match="xsd:element[@name='while']" />
  <xsl:template match="xsd:complexType[@name='tWhile']" />

  <!-- REMOVE SEQUENCE -->
  <xsl:template match="xsd:element[@ref='sequence']" />
  <xsl:template match="xsd:element[@name='sequence']" />
  <xsl:template match="xsd:complexType[@name='tSequence']" />


  <!-- DEFAULT VALUES -->

  <!-- Make initiate attribute on correlations mandatory -->
  <xsl:template match="xsd:attribute[@name='initiate' and type='tInitiate']">
      <xsd:attribute name="initiate" type="tInitiate" use="required" />
  </xsl:template>  

  <!-- Make expressionLanguage attribute a fixed mandatory -->
  <xsl:template match="xsd:attribute[@name='expressionLanguage']">
      <xsd:attribute name="expressionLanguage" type="xsd:anyURI" fixed="urn:oasis:names:tc:wsbpel:2.0:sublang:xpath1.0"/>
  </xsl:template>  

  <!-- Make queryLanguage attribute a fixed mandatory -->
  <xsl:template match="xsd:attribute[@name='queryLanguage']">
      <xsd:attribute name="queryLanguage" type="xsd:anyURI" fixed="urn:oasis:names:tc:wsbpel:2.0:sublang:xpath1.0"/>
  </xsl:template>  

  <!-- Make suppressJoinFailure mandatory without default value -->
  <xsl:template match="xsd:attribute[@name='suppressJoinFailure']">
    <xsd:attribute name="suppressJoinFailure" type="tBoolean" use="required" />
  </xsl:template>

  <!-- Make exitOnStandardFailure mandatory without default value -->
  <xsl:template match="xsd:attribute[@name='exitOnStandardFault']">
    <xsd:attribute name="exitOnStandardFault" type="tBoolean" use="required" />
  </xsl:template>

  <!-- Make joinCondition mandatory -->
  <xsl:template match="xsd:element[@ref='joinCondition']">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:attribute name="minOccurs">1</xsl:attribute>
    </xsl:copy>
  </xsl:template>

  <!-- Make transitionCondition mandatory -->
  <xsl:template match="xsd:element[@ref='transitionCondition']">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:attribute name="minOccurs">1</xsl:attribute>
    </xsl:copy>
  </xsl:template>

  <!-- Make fault handlers mandatory -->
  <xsl:template match="xsd:complexType[@name='tScope']//xsd:element[@ref='faultHandlers']">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:attribute name="minOccurs">1</xsl:attribute>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="xsd:complexType[@name='tFaultHandlers']/xsd:element[@ref='catchAll']">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:attribute name="minOccurs">1</xsl:attribute>
    </xsl:copy>
  </xsl:template>
  
  <!-- Make compensation handlers mandatory -->
  <xsl:template match="xsd:element[@ref='compensationHandler']">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:attribute name="minOccurs"><xsl:value-of select="1" /></xsl:attribute>
    </xsl:copy>
  </xsl:template>

  <!-- Make termination handlers mandatory -->
  <xsl:template match="xsd:element[@ref='terminationHandler']">
    <xsl:copy>
      <xsl:copy-of select="@*"/>
      <xsl:attribute name="minOccurs"><xsl:value-of select="1" /></xsl:attribute>
    </xsl:copy>
  </xsl:template>
  
  <!-- Remove default value from keepSrcElementName attribute on copy -->
  <xsl:template match="xsd:attribute[@name='keepSrcElementName']">
    <xsd:attribute name="keepSrcElementName" type="tBoolean" use="optional"/>
  </xsl:template>
  
</xsl:stylesheet>