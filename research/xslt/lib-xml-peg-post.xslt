<xsl:transform
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:local="file:test.xslt"
        version="2.0">
    <xsl:output method="xml" standalone="yes" indent="yes"/>
    <xsl:template match="sequence[empty(child::element())]" priority="1"/>
    <xsl:template match="sequence/sequence[not(@name) or @name = parent::sequence/@name]" priority="0.9">
        <xsl:apply-templates select="child::node()"/>
    </xsl:template>
    <xsl:template match="sequence/*" priority="0.8">
        <xsl:call-template name="default"/>
    </xsl:template>
    <xsl:template name="default" match="node() | attribute()">
        <xsl:copy>
            <xsl:apply-templates select="node() | attribute()"/>
        </xsl:copy>
    </xsl:template>
</xsl:transform>
