<xsl:transform
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        version="2.0">

    <xsl:output method="xml" standalone="yes" indent="yes"/>

    <!-- Number structural elements. -->

    <xsl:template match="part">
        <xsl:copy>
            <xsl:attribute name="number">
                <xsl:number count="part"/>
            </xsl:attribute>
            <xsl:call-template name="default"/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match="chapter[@numbered]">
        <xsl:copy>
            <xsl:attribute name="number">
                <xsl:number level="any" count="chapter[@numbered]"/>
            </xsl:attribute>
            <xsl:call-template name="default"/>
        </xsl:copy>
    </xsl:template>
    <xsl:template match="chapter[@numbered]/section[@numbered]">
        <xsl:copy>
            <xsl:attribute name="number">
                <xsl:number count="section[@numbered]"/>
            </xsl:attribute>
            <xsl:call-template name="default"/>
        </xsl:copy>
    </xsl:template>

    <xsl:include href="default-copy.xslt"/>

</xsl:transform>
