<?xml version="1.0" encoding="UTF-8"?>
<!--
    Translate math markup to HTML+CSS.
-->
<xsl:transform
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        version="2.0">

    <xsl:output method="xml" standalone="yes" indent="yes"/>

    <!-- inline math -->
    <xsl:template match="math">
        <span class="math"><xsl:apply-templates/></span>
    </xsl:template>

    <!-- identifier, operator, number -->
    <xsl:template match="mi"><span class="identifier"><xsl:apply-templates/></span></xsl:template>
    <xsl:template match="mo"><span class="operator arity-{@arity}"><xsl:apply-templates/></span></xsl:template>
    <xsl:template match="mn"><span class="number"><xsl:apply-templates/></span></xsl:template>
    <xsl:template match="msub">
        <xsl:apply-templates select="element()[1]"/>
        <sub>
            <xsl:apply-templates select="element()[2]"/>
        </sub>
    </xsl:template>
    <xsl:template match="msup">
        <xsl:apply-templates select="element()[1]"/>
        <sup>
            <xsl:apply-templates select="element()[2]"/>
        </sup>
    </xsl:template>

    <xsl:include href="default-copy.xslt"/>

</xsl:transform>
