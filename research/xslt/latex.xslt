<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        version="2.0">
    <xsl:output method="text"/>
    <xsl:strip-space elements="*"/>

    <xsl:template match="/book">
% draft is faster than final?
% set to 10pt if using fonts with big x-height such as arev
%\documentclass[10pt,openany]{memoir}
\documentclass[12pt,twoside,openany,draft]{memoir}
\usepackage{mystyle}
\usepackage[inline]{enumitem}% enumerate*; should remove?

%
% citation
%\usepackage{cite}% replaced with biblatex
\usepackage[backend=biber]{biblatex}
\addbibresource{../bib.bib}% relative to biber working directory
\def\MyCite#1{\footnote{\cite{#1} \fullcite{#1}}}
\def\MyCitePage#1#2{\footnote{\cite{#1} \footfullcite[][p.~#2]{#1}}}

\makeindex
\begin{document}
\frontmatter
\begin{titlingpage}
    {
        \centering
        \def\usefontsize#1{\fontsize{#1}{#1}\selectfont}
        \usefontsize{96pt}Studies\par
        %\usefontsize{48pt}from concrete to abstract\par
        \vspace*{\fill}
        \usefontsize{36pt}2017 Draft Edition\par%2017-11-01
        \vspace*{\fill}
        \vspace*{\fill}
        \usefontsize{24pt}Erik Dominikus\par
    }
    \clearpage
    \input{copyright}
\end{titlingpage}

\clearpage

\long\def\disabled#1{}

    <xsl:apply-templates/>

\end{document}
    </xsl:template>
    <xsl:template match="/book/front-matter">
        <xsl:apply-templates/>
    </xsl:template>
    <!-- Not implemented. -->
    <xsl:template match="book/title"/>
    <xsl:template match="book/edition"/>
    <xsl:template match="book/author"/>
    <xsl:template match="book/table-of-contents">
{
    \makeatletter
    {
        \clearpage
        \ListOfParts
    }
    {
        \clearpage
        \ListOfChapters
    }
    {
        \clearpage
        \renewcommand\contentsname{Table of contents}
        \maxtocdepth{subsection}
        \tableofcontents
    }
    \renewcommand\listtablename{List of tables}
    \renewcommand\listfigurename{List of figures}
    \listoftables
    \listoffigures
    \makeatother
}
% this should not be in the table-of-contents template rule
\mainmatter
    </xsl:template>

    <!-- structural elements -->
    <xsl:template match="part">
        \part{<xsl:apply-templates select="title"/> }
        \PartTableOfContents
        <xsl:apply-templates/>
    </xsl:template>
    <xsl:template match="part/title"><xsl:apply-templates/></xsl:template>
    <xsl:template match="chapter">
        \chapter{<xsl:value-of select="title"/>}
        <xsl:if test="@id">
            \label{<xsl:value-of select="@id"/>}
        </xsl:if>
        <xsl:apply-templates/>
    </xsl:template>
    <xsl:template match="chapter/title"/>

    <xsl:template match="section">
        \section{<xsl:value-of select="title"/>}
        <xsl:if test="@id">
            \label{<xsl:value-of select="@id"/>}
        </xsl:if>
        <xsl:apply-templates/>
    </xsl:template>
    <xsl:template match="section/title"/>

    <!-- a "paragraph" is textualized and passed verbatim to latex -->
    <xsl:template match="paragraph"><xsl:apply-templates/>&#x0A;&#x0A;</xsl:template>

    <xsl:template match="exercises">
        <xsl:apply-templates/>
        <xsl:text>\ShowAnswers&#x0A;</xsl:text>
    </xsl:template>
    <xsl:template match="exercise">
        <xsl:text>\ExerciseAnswer{</xsl:text>
        <xsl:apply-templates select="problem"/>
        <xsl:text>}{</xsl:text>
        <xsl:apply-templates select="answer"/>
        <xsl:text>}&#x0A;</xsl:text>
    </xsl:template>
    <xsl:template match="exercise/problem | exercise/answer"><xsl:apply-templates/></xsl:template>

    <xsl:template match="enquote">
        <xsl:text>\enquote{</xsl:text>
        <xsl:apply-templates/>
        <xsl:text>}</xsl:text>
    </xsl:template>

    <!-- inline math -->
    <xsl:template match="math">\(<xsl:apply-templates/>\)</xsl:template>
    <!-- math identifier -->
    <xsl:template match="paragraph//mi">\(<xsl:apply-templates/>\)</xsl:template>

    <!-- display math -->
    <xsl:template match="math-block">
        \begin{align}
            <xsl:apply-templates/>
        \end{align}
    </xsl:template>
    <xsl:template match="math-block/line">
        <xsl:if test="position() ne 1">\\ </xsl:if>
        <xsl:apply-templates/>
        <xsl:text>&#x0A;</xsl:text>
    </xsl:template>
    <xsl:template match="math-block/equation">
        <xsl:apply-templates select="left"/> = <xsl:apply-templates select="right"/>
    </xsl:template>
    <xsl:template match="equation/left | equation/right"><xsl:apply-templates/></xsl:template>

    <!-- mathematical symbols -->
    <xsl:template match="set-real"> \mathbb{R} </xsl:template>

    <!-- mathematical expressions -->
    <xsl:template match="factorial"><xsl:apply-templates/>!</xsl:template>
    <xsl:template match="element">
        <xsl:apply-templates select="object"/> \in <xsl:apply-templates select="set"/>
    </xsl:template>
    <xsl:template match="element/object | element/set"><xsl:apply-templates/></xsl:template>
    <xsl:template match="element-of"> \in </xsl:template>

    <xsl:template match="citation">
        <xsl:text>\FootnoteUrl{</xsl:text>
        <xsl:value-of select="replace(replace(@href, '%', '\\%'), '#', '\\#')"/>
        <xsl:text>}</xsl:text>
    </xsl:template>

    <xsl:template match="bibliographies"/>

    <xsl:include href="default-reject.xslt"/>
</xsl:transform>
