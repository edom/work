<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:ca="file:/default-reject.xslt"
        version="2.0">
    <xsl:template match="*">
        <xsl:message terminate="yes">
            <xsl:text>Unexpected element: </xsl:text>
            <xsl:call-template name="ca:path"/>
        </xsl:message>
    </xsl:template>
    <xsl:template name="ca:path">
        <xsl:for-each select="..">
            <xsl:call-template name="ca:path"/>
        </xsl:for-each>
        <xsl:text>/</xsl:text>
        <xsl:value-of select="name()"/>
        <xsl:text>[</xsl:text>
        <xsl:value-of select="position()"/>
        <xsl:text>]</xsl:text>
    </xsl:template>
</xsl:transform>
