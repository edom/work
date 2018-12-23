<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        version="2.0">

    <xsl:param name="mathjax" required="yes" as="xs:boolean"/>

    <xsl:output method="html"/>

    <xsl:strip-space elements="*"/>

    <xsl:template match="/book">
        <html>
            <head>
                <title><xsl:value-of select="book/title"/></title>
                <link rel="stylesheet" type="text/css" href="../style.css"/>
                <xsl:if test="$mathjax">
                    <script src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-MML-AM_CHTML"/>
                </xsl:if>
            </head>
            <body class="book">
                <xsl:apply-templates/>
            </body>
        </html>
    </xsl:template>
    <xsl:template match="/book/title"><h1 class="title"><xsl:apply-templates/></h1></xsl:template>
    <xsl:template match="/book/edition"><h2 class="edition"><xsl:apply-templates/></h2></xsl:template>
    <xsl:template match="/book/author"><h3 class="author"><xsl:apply-templates/></h3></xsl:template>
    <xsl:template match="/book/front-matter"><xsl:apply-templates/></xsl:template>

    <xsl:variable name="toc-id" select="'table-of-contents'"/>

    <xsl:template match="/book/table-of-contents">
        <div class="toc" id="{$toc-id}">
            <xsl:call-template name="navigation"/>
            <h1 class="title">Table of contents</h1>
            <xsl:apply-templates select="//(part|chapter|section)" mode="toc"/>
        </div>
    </xsl:template>
    <xsl:template match="part" mode="toc">
        <div class="line part">
            <span class="number"><xsl:value-of select="@number"/></span>
            <a class="title" href="#{@id}">
                <xsl:apply-templates select="title/node()"/>
            </a>
        </div>
    </xsl:template>
    <xsl:template match="chapter" mode="toc">
        <div class="line chapter">
            <span class="number"><xsl:value-of select="@number"/></span>
            <a class="title" href="#{@id}">
                <xsl:apply-templates select="title/node()"/>
            </a>
        </div>
    </xsl:template>
    <xsl:template match="section" mode="toc">
        <div class="line section">
            <span class="number"><xsl:value-of select="concat(parent::*/@number, '.', @number)"/></span>
            <a class="title" href="#{@id}">
                <xsl:apply-templates select="title/node()"/>
            </a>
        </div>
    </xsl:template>

    <!-- structural elements -->

    <xsl:template name="navigation">
        <xsl:variable name="previous-chapter" select="preceding::chapter[1]"/>
        <xsl:variable name="next-chapter" select="following::chapter[1]"/>
        <div class="navigation">
            <div>
                <a href="#{$toc-id}">Contents</a>
            </div>
            <div>
                <xsl:text>Previous: </xsl:text>
                <xsl:apply-templates select="$previous-chapter" mode="navigation"/>
            </div>
            <div>
                <xsl:text>Current: </xsl:text>
                <xsl:apply-templates select="." mode="navigation"/>
            </div>
            <div>
                <xsl:text>Next: </xsl:text>
                <xsl:apply-templates select="$next-chapter" mode="navigation"/>
            </div>
        </div>
    </xsl:template>
    <xsl:template match="chapter" mode="navigation">
        <a href="#{@id}">
            <xsl:apply-templates select="@number" mode="navigation"/>
            <xsl:apply-templates select="title" mode="navigation"/>
        </a>
    </xsl:template>
    <xsl:template match="chapter/@number" mode="navigation">
        <xsl:text>Chapter </xsl:text>
        <xsl:value-of select="."/>
        <xsl:text>: </xsl:text>
    </xsl:template>
    <xsl:template match="title" mode="navigation">
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="exercises">
        <div class="exercises">
            <xsl:apply-templates select="exercise/problem"/>
            <xsl:apply-templates select="exercise/answer"/>
        </div>
    </xsl:template>
    <xsl:template match="exercise/problem">
        <div class="problem">
            <span class="label">Exercise <xsl:value-of select="../@number"/></span>
            <xsl:apply-templates/>
        </div>
    </xsl:template>
    <xsl:template match="exercise/answer">
        <div class="answer">
            <span class="label">Answer <xsl:value-of select="../@number"/></span>
            <xsl:apply-templates/>
        </div>
    </xsl:template>

    <!-- Output of html-cite.xslt. -->
    <xsl:template match="citation">
        <sup id="footnote-{@number}-occurrence">
            <a href="#footnote-{@number}-content">
                <xsl:text>[</xsl:text>
                <xsl:value-of select="@number"/>
                <xsl:text>]</xsl:text>
            </a>
        </sup>
    </xsl:template>

    <xsl:template match="part">
        <div class="part" id="{@id}">
            <xsl:apply-templates/>
        </div>
    </xsl:template>
    <xsl:template match="part/title">
        <h1 class="title-block">
            <xsl:apply-templates select="../@number"/>
            <span class="title"><xsl:apply-templates/></span>
        </h1>
    </xsl:template>
    <xsl:template match="part/@number">
        <span class="number">Part <xsl:value-of select="."/></span>
    </xsl:template>

    <xsl:template match="chapter">
        <div class="chapter" id="{@id}">
            <xsl:call-template name="navigation"/>
            <xsl:apply-templates/>
            <div class="footer">
                <xsl:for-each select=".//citation">
                    <div id="footnote-{@number}-content">
                        <a href="#footnote-{@number}-occurrence">^</a>
                        <xsl:text> [</xsl:text>
                        <xsl:value-of select="@number"/>
                        <xsl:text>] </xsl:text>
                        <a href="{@href}"><xsl:value-of select="@href"/></a>
                    </div>
                </xsl:for-each>
            </div>
        </div>
    </xsl:template>
    <xsl:template match="chapter/title">
        <h1 class="title-block">
            <xsl:apply-templates select="../@number"/>
            <span class="title"><xsl:apply-templates/></span>
        </h1>
    </xsl:template>
    <xsl:template match="chapter/@number">
        <span class="number"><xsl:value-of select="."/></span>
    </xsl:template>
    <xsl:template match="chapter/abstract">
        <div class="abstract">
            <div class="title">Abstract</div>
            <xsl:apply-templates/>
        </div>
    </xsl:template>

    <xsl:template match="section">
        <div class="section" id="{@id}">
            <xsl:apply-templates/>
        </div>
    </xsl:template>
    <xsl:template match="section/title">
        <h2 class="title-block">
            <span class="number">
                <xsl:value-of select="../../@number"/>
                <xsl:text>.</xsl:text>
                <xsl:value-of select="../@number"/>
            </span>
            <span class="title"><xsl:apply-templates/></span>
        </h2>
    </xsl:template>

    <!-- a "paragraph" is textualized and passed verbatim to latex -->
    <xsl:template match="paragraph"><p><xsl:apply-templates/></p>&#x0A;</xsl:template>
    <xsl:template match="emphasis"><em><xsl:apply-templates/></em></xsl:template>

    <!-- U+201C and U+201D are left and right double quotation mark -->
    <xsl:template match="enquote">
        <xsl:text>&#x201C;</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>&#x201D;</xsl:text>
    </xsl:template>

    <!-- Embedded HTML subset generated by lib-xml-math-html-css.xslt -->
    <xsl:template match="span | sup | sub">
        <xsl:copy-of select="."/>
    </xsl:template>

    <xsl:template match="*">
        <xsl:message terminate="yes">
            Unexpected element: <xsl:value-of select="name()"/>
        </xsl:message>
    </xsl:template>

</xsl:transform>
