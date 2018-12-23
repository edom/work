<xsl:transform
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:local="file:/lib-xml-normalize.xslt"
        version="2.0"
        exclude-result-prefixes="#all">

    <xsl:output method="xml" standalone="yes" indent="yes"/>

    <!-- Normalize @numbered attributes. -->

    <xsl:template match="part | chapter | section">
        <xsl:copy>
            <xsl:if test="not(@numbered) or @numbered = 'true'">
                <xsl:attribute name="numbered" select="'numbered'"/>
            </xsl:if>
            <xsl:if test="not(@id)">
                <xsl:attribute name="id" select="generate-id()"/>
            </xsl:if>
            <xsl:call-template name="default"/>
        </xsl:copy>
    </xsl:template>
    <xsl:template match="part/@numbered | chapter/@numbered | section/@numbered"/>

    <!-- Ensure that inline math children are in math element. -->

    <xsl:template match="fn | mi | mn | mo | msub | msup
    | mplus | mminus | mtimes | mcdot
    | equation | raise | apply | factorial">
        <xsl:choose>
            <xsl:when test="ancestor::math">
                <xsl:copy-of select="."/>
            </xsl:when>
            <xsl:otherwise>
                <math>
                    <xsl:copy-of select="."/>
                </math>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>


    <!--
    Flatten paragraphs.
    Remove empty paragraphs.
    -->

    <xsl:function name="local:paragraph">
        <xsl:param name="content" as="item()*"/>
        <xsl:param name="remaining" as="item()*"/>
        <xsl:variable name="emit">
            <xsl:if test="$content">
                <paragraph>
                    <xsl:sequence select="$content"/>
                </paragraph>
            </xsl:if>
        </xsl:variable>
        <xsl:variable name="head" select="$remaining[1]"/>
        <xsl:variable name="tail" select="$remaining[position() > 1]"/>
        <xsl:choose>
            <xsl:when test="empty($remaining)">
                <xsl:sequence select="$emit"/>
            </xsl:when>
            <xsl:when test="$head/self::paragraph">
                <xsl:sequence select="$emit"/>
                <xsl:sequence select="local:paragraph((), $head/node())"/>
                <xsl:sequence select="local:paragraph((), $tail)"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:sequence select="local:paragraph(($content, $head), $tail)"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>

    <xsl:template match="paragraph">
        <!--<xsl:sequence select="local:paragraph((), .)"/>-->
        <xsl:apply-templates mode="after-unnest" select="local:paragraph((), .)"/>
    </xsl:template>

    <xsl:template mode="after-unnest" match="paragraph">
        <xsl:apply-templates mode="after-unnest"/>
    </xsl:template>
    <xsl:template mode="after-unnest" match="*">
        <xsl:apply-templates mode="#default" select="."/>
    </xsl:template>


    <xsl:include href="default-copy.xslt"/>

</xsl:transform>
