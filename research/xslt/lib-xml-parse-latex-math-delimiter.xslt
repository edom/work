<xsl:transform
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:local="lib-xml-parse-latex-math-delimiter.xslt"
        version="2.0"
        exclude-result-prefixes="#all">

    <xsl:output method="xml" standalone="yes" indent="yes"/>

    <xsl:template match="paragraph/text()">
        <xsl:analyze-string
                select="."
                regex="\\\((.*?)\\\)"
                flags="m">
            <xsl:matching-substring>
                <xsl:variable name="content" select="regex-group(1)"/>
                <xsl:if test="matches($content, '\\\(')">
                    <xsl:message terminate="yes">Can't have \( in \(.</xsl:message>
                </xsl:if>
                <math type="latex">
                    <xsl:sequence select="$content"/>
                </math>
            </xsl:matching-substring>
            <xsl:non-matching-substring>
                <xsl:sequence select="."/>
            </xsl:non-matching-substring>
        </xsl:analyze-string>
    </xsl:template>

    <xsl:include href="default-copy.xslt"/>

</xsl:transform>
