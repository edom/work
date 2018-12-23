<xsl:transform
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:t="lib-xml-parse-tex-paragraph.xslt"
        version="2.0"
        exclude-result-prefixes="#all">

    <xsl:output method="xml" standalone="yes" indent="yes"/>

    <!--
    Split TeX paragraph into XML paragraphs.

    Example:

        <paragraph>
        Par 1

        Par 2
        </paragraph>

    becomes something like

        <paragraph>
            <paragraph>Par 1</paragraph>
            <paragraph>Par 2</paragraph>
        </paragraph>

    modulo whitespaces.
    -->

    <xsl:template match="chapter/text() | paragraph/text()">
        <xsl:analyze-string
                select="."
                regex="(\s*)(.*?)[\n\r][\n\r](\s*)"
                flags="m">
            <xsl:matching-substring>
                <paragraph>
                    <xsl:sequence select="regex-group(2)"/>
                </paragraph>
            </xsl:matching-substring>
            <xsl:non-matching-substring>
                <xsl:sequence select="."/>
            </xsl:non-matching-substring>
        </xsl:analyze-string>
    </xsl:template>

    <xsl:include href="default-copy.xslt"/>

</xsl:transform>
