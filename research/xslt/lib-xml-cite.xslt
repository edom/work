<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        version="2.0">

    <xsl:output method="xml" standalone="yes" indent="yes"/>

    <xsl:key name="key-bibliography" match="bibliography" use="@key"/>

    <!-- Resolve 'cite' to 'citation'. -->
    <xsl:template match="cite">
        <xsl:variable name="key" select="@key"/>
        <xsl:variable name="bib" select="key('key-bibliography', $key)"/>
        <xsl:variable name="url" select="$bib/url"/>
        <xsl:if test="not($bib)">
            <!--
                How do we print the location of the offending cite tag
                (in the offending XML file)?
            -->
            <xsl:message terminate="yes">Unknown bibliography key: <xsl:value-of select="$key"/></xsl:message>
        </xsl:if>
        <citation href="{$url}">
            <xsl:attribute name="number"><xsl:number/></xsl:attribute>
        </citation>
    </xsl:template>

    <!-- Remove these elements from the output. -->
    <xsl:template match="bibliographies"/>

    <xsl:include href="default-copy.xslt"/>

</xsl:transform>
