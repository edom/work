<?xml version="1.0" encoding="UTF-8"?>
<!--
    Translate math markup to LaTeX math fragment.
    Can also be used with html.xslt with MathJax.
-->
<xsl:transform
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        version="2.0">

    <xsl:output method="xml" standalone="yes" indent="yes"/>

    <!-- inline LaTeX math fragment -->
    <xsl:template match="math">
        <xsl:text>\(</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>\)</xsl:text>
    </xsl:template>

    <!-- nested math should not print \( \) -->
    <xsl:template match="math//math"><xsl:apply-templates/></xsl:template>
    <xsl:template match="math-block//math"><xsl:apply-templates/></xsl:template>

    <!-- identifier, operator, number -->
    <xsl:template match="mi | mo | mn"><xsl:apply-templates/></xsl:template>

    <!-- anonymous function expression -->
    <xsl:template match="fn">
        <xsl:apply-templates select="element()[1]"/>
        <xsl:text> \to </xsl:text>
        <xsl:apply-templates select="element()[2]"/>
    </xsl:template>

    <!-- display math -->
    <xsl:template match="math-block">
        <xsl:text>\begin{align}</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>\end{align}</xsl:text>
    </xsl:template>
    <xsl:template match="math-block/line">
        <xsl:apply-templates/>
    </xsl:template>
    <xsl:template match="math-block/equation">
        <xsl:apply-templates select="element()[1]"/>
        <xsl:text> &amp;= </xsl:text>
        <xsl:apply-templates select="element()[2]"/>
    </xsl:template>

    <xsl:include href="default-copy.xslt"/>

</xsl:transform>
