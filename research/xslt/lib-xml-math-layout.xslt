<?xml version="1.0" encoding="UTF-8"?>
<!--
    Translate content-oriented math to presentation-oriented math.
-->
<xsl:transform
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:f="file:/lib-xml-math-layout.xslt"
        version="2.0">

    <xsl:output method="xml" standalone="yes" indent="yes"/>

    <!-- https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode#Mathematical_Operators_block -->
    <xsl:variable name="map-operator">
        <entry key="equation" value="="/>
        <entry key="fn" value="&#x2192;"/><!-- rightwards arrow -->
        <entry key="mplus" value="+"/>
        <entry key="mminus" value="&#x2212;"/><!-- minus sign -->
        <entry key="mtimes" value="&#xD7;"/><!-- multiplication sign -->
        <entry key="mcdot" value="&#x22C5;"/><!-- dot operator -->
    </xsl:variable>

    <xsl:function name="f:operator-binary">
        <xsl:param name="context"/>
        <xsl:apply-templates select="$context/element()[1]"/>
        <mo arity="2"><xsl:value-of select="$map-operator/entry[@key = name($context)]/@value"/></mo>
        <xsl:apply-templates select="$context/element()[2]"/>
    </xsl:function>

    <xsl:template match="fn | equation | mplus | mminus | mtimes | mcdot">
        <xsl:sequence select="f:operator-binary(.)"/>
    </xsl:template>

    <xsl:template match="factorial"><xsl:apply-templates/><mo arity="1">!</mo></xsl:template>

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
