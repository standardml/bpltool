<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- Remove all uses of optional extensions. -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:bpel="http://docs.oasis-open.org/wsbpel/2.0/process/executable">

  <xsl:output indent="yes" method="xml" />
  
  <!-- Leave out optional elements. -->
  <!-- NB: extensions may use the same namespace and mandatory extensions takes precedence. -->
  <xsl:template match="*[namespace-uri() = /bpel:process/bpel:extensions/bpel:extension[@mustUnderstand='no']/@namespace and
                         not(namespace-uri() = /bpel:process/bpel:extensions/bpel:extension[@mustUnderstand='yes']/@namespace)]" />
  
  <!-- Copy all other elements. -->
  <xsl:template match="*">
    <xsl:copy>
	    <xsl:copy-of select="@*[not(namespace-uri() = /bpel:process/bpel:extensions/bpel:extension[@mustUnderstand='no']/@namespace) or
	                            namespace-uri() = /bpel:process/bpel:extensions/bpel:extension[@mustUnderstand='yes']/@namespace]" />
      <xsl:apply-templates />
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>
