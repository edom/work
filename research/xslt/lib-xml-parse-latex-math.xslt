<xsl:transform
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:local="file:/lib-xml-parse-latex-math.xslt"
        version="2.0"
        exclude-result-prefixes="#all">
    <xsl:output method="xml" standalone="yes" indent="yes"/>

    <!-- The TeXbook page 37 -->
    <xsl:variable name="local:catcodes">
        <catcode number="0" members="\"/><!-- escape character -->
        <catcode number="1" members="{{"/><!-- beginning of group -->
        <catcode number="2" members="}}"/><!-- end of group -->
        <catcode number="3" members="$"/><!-- math shift -->
        <catcode number="4" members="&amp;"/><!-- alignment tab -->
        <catcode number="5" members="\n\r"/><!-- end of line -->
        <catcode number="6" members="#"/><!-- parameter -->
        <catcode number="7" members="^"/><!-- superscript -->
        <catcode number="8" members="_"/><!-- subscript -->
        <catcode number="9" members=""/><!-- ignored -->
        <catcode number="10" members=" "/><!-- space -->
        <catcode number="11" members="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"/><!-- letter -->
        <catcode number="12" members="!&quot;'()*+,-./0123456789:;&lt;=&gt;?@[]`|"/><!-- other character -->
        <catcode number="13" members="~"/><!-- active character -->
        <catcode number="14" members="%"/><!-- comment character -->
        <catcode number="15" members="&#x7F;"/><!-- invalid character -->
    </xsl:variable>

    <xsl:function name="local:catcode">
        <xsl:param name="char" as="xs:string"/>
        <xsl:sequence select="$local:catcodes/catcode[contains(@members, $char)][1]/@number"/>
    </xsl:function>

    <!--
    TeX reader
    read('abc')
    -->
    <xsl:function name="local:read">
        <xsl:param name="input" as="xs:string"/>
        <xsl:variable name="codepoints" select="string-to-codepoints($input)"/>
        <xsl:variable name="chars">
            <xsl:for-each select="$codepoints">
                <char string="{codepoints-to-string(.)}"/>
            </xsl:for-each>
        </xsl:variable>
        <xsl:for-each select="$chars/char/@string">
            <xsl:variable name="char" select="."/>
            <char catcode="{local:catcode($char)}" string="{$char}"/>
        </xsl:for-each>
    </xsl:function>

    <xsl:function name="local:tokenize">
        <!-- sequence of <char> elements; return value of read -->
        <xsl:param name="chars" as="element()*"/>
        <xsl:sequence select="local:tokenize-0((), $chars)"/>
    </xsl:function>

    <xsl:function name="local:tokenize-0">
        <xsl:param name="current" as="element()*"/>
        <xsl:param name="chars" as="element()*"/>
        <xsl:variable name="head" select="$chars[1]"/>
        <xsl:variable name="tail" select="$chars[position() > 1]"/>
        <xsl:choose>
            <xsl:when test="empty($chars)">
                <xsl:for-each select="$current/@string">
                    <xsl:value-of select="."/>
                </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
                <xsl:variable name="hc" select="$head/@catcode"/>
                <xsl:variable name="catcode-escape" select="0"/>
                <xsl:choose>
                    <xsl:when test="$hc = $catcode-escape">
                    </xsl:when>
                </xsl:choose>
                <xsl:sequence select="local:tokenize-0(($current, $head), $tail)"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>

    <xsl:template match="latex">
        <!--
        <xsl:variable name="list" select="local:explode(text())"/>
        <xsl:sequence select="local:parse($list)"/>
        -->
        <xsl:variable name="read" select="local:read(text())"/>
        <latex>
            <read>
                <xsl:sequence select="$read"/>
            </read>
            <tokenize>
                <xsl:sequence select="local:tokenize($read)"/>
            </tokenize>
        </latex>
    </xsl:template>

    <!--
        explode('abc')
        <char value="a"/>
        <char value="b"/>
        <char value="c"/>
    -->
    <xsl:function name="local:explode">
        <xsl:param name="string"/>
        <xsl:for-each select="string-to-codepoints($string)">
            <char value="{codepoints-to-string(.)}"/>
        </xsl:for-each>
    </xsl:function>

    <!-- separate lexing and parsing; follow TeX algorithm -->
    <!-- parse(explode($string)) -->
    <xsl:function name="local:parse">
        <xsl:param name="chars"/>
        <xsl:variable name="head" select="$chars[1]/@value"/>
        <xsl:variable name="tail" select="$chars[position() > 1]"/>
        <xsl:choose>
            <xsl:when test="empty($chars)"/>
            <xsl:when test="$head = '{'">
                <xsl:variable name="subresult" select="local:parse-group($tail)"/>
                <xsl:sequence select="$subresult/group"/>
                <xsl:sequence select="local:parse($subresult/unparsed/*)"/>
            </xsl:when>
            <xsl:when test="$head = '}'">
                <xsl:message terminate="yes">Unexpected end-group } without matching begin-group</xsl:message>
            </xsl:when>
            <xsl:when test="$head = '\'">
                <xsl:variable name="subresult" select="local:build-command-name($tail)"/>
                <command name="{$subresult/@name}"/>
                <xsl:sequence select="local:parse($subresult/unparsed/*)"/>
            </xsl:when>
            <xsl:when test="$head = '^'">
                <xsl:variable name="subresult" select="local:parse-argument($tail)"/>
                <superscript>
                    <xsl:sequence select="$subresult/group"/>
                </superscript>
                <xsl:sequence select="local:parse($subresult/unparsed/*)"/>
            </xsl:when>
            <xsl:when test="$head = '_'">
                <xsl:variable name="subresult" select="local:parse-argument($tail)"/>
                <subscript>
                    <xsl:sequence select="$subresult/group"/>
                </subscript>
                <xsl:sequence select="local:parse($subresult/unparsed/*)"/>
            </xsl:when>
            <xsl:when test="$head = '~'">
                <non-breaking-space string="{$head}"/>
                <xsl:sequence select="local:parse($tail)"/>
            </xsl:when>
            <xsl:when test="matches($head, '[A-Za-z]')">
                <letter string="{$head}"/>
                <xsl:sequence select="local:parse($tail)"/>
            </xsl:when>
            <xsl:when test="matches($head, '[()+/=&lt;&gt;-]')">
                <operator string="{$head}"/>
                <xsl:sequence select="local:parse($tail)"/>
            </xsl:when>
            <xsl:when test="matches($head, '[0-9]')">
                <digit string="{$head}"/>
                <xsl:sequence select="local:parse($tail)"/>
            </xsl:when>
            <xsl:when test="matches($head, '[ \n\r\t]')">
                <xsl:sequence select="local:parse($tail)"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:message terminate="yes">
                    <xsl:text>Illegal character: </xsl:text>
                    <xsl:value-of select="$head"/>
                    <xsl:text> (codepoint </xsl:text>
                    <xsl:value-of select="string-to-codepoints($head)"/>
                    <xsl:text>)</xsl:text>
                </xsl:message>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>

    <xsl:function name="local:build-command-name">
        <xsl:param name="chars"/>
        <xsl:variable name="head" select="$chars[1]/@value"/>
        <xsl:variable name="tail" select="$chars[position() > 1]"/>
        <xsl:choose>
            <xsl:when test="matches($head, '[A-Za-z]')">
                <xsl:variable name="subresult" select="local:build-command-name($tail)"/>
                <xsl:variable name="name" select="concat($head, $subresult/@name)"/>
                <result name="{$name}">
                    <xsl:sequence select="$subresult/unparsed"/>
                </result>
            </xsl:when>
            <xsl:otherwise>
                <result name="">
                    <unparsed><xsl:sequence select="$chars"/></unparsed>
                </result>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>

    <xsl:function name="local:parse-argument">
        <xsl:param name="chars"/>
        <xsl:variable name="head" select="$chars[1]/@value"/>
        <xsl:variable name="tail" select="$chars[position() > 1]"/>
        <xsl:choose>
            <xsl:when test="$head = '{'">
                <xsl:sequence select="local:parse-group($tail)"/>
            </xsl:when>
            <xsl:when test="$head = '}'">
                <xsl:message terminate="yes">Unexpected end group }.</xsl:message>
            </xsl:when>
            <xsl:when test="matches($head, '[ \n\r\t]')">
                <xsl:sequence select="local:parse-argument($tail)"/>
            </xsl:when>
            <xsl:otherwise>
                <result>
                    <group><xsl:sequence select="$chars[1]"/></group>
                    <unparsed><xsl:sequence select="$tail"/></unparsed>
                </result>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>

    <xsl:function name="local:parse-group">
        <xsl:param name="chars"/>
        <xsl:variable name="head" select="$chars[1]/@value"/>
        <xsl:variable name="tail" select="$chars[position() > 1]"/>
        <xsl:choose>
            <xsl:when test="$head = '}'">
                <result>
                    <group/>
                    <unparsed><xsl:sequence select="$tail"/></unparsed>
                </result>
            </xsl:when>
            <xsl:otherwise>
                <result>
                    <xsl:variable name="subresult" select="local:parse-group($tail)"/>
                    <group>
                        <xsl:sequence select="($chars[1], $subresult/group/*)"/>
                    </group>
                    <xsl:sequence select="$subresult/unparsed"/>
                </result>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:function>

    <xsl:template match="node() | attribute()">
        <xsl:copy>
            <xsl:apply-templates select="node() | attribute()"/>
        </xsl:copy>
    </xsl:template>
</xsl:transform>
