<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        version="2.0">
    <!-- Copy everything else unchanged. -->
    <xsl:template match="node() | attribute()">
        <xsl:copy>
            <xsl:call-template name="default"/>
        </xsl:copy>
    </xsl:template>
    <xsl:template name="default">
        <xsl:apply-templates select="child::node() | attribute()"/>
    </xsl:template>
</xsl:transform>
