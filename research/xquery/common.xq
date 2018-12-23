xquery version "3.0";

module namespace common = "com.spacetimecat.xquery.common";

declare %private function common:is_content_math_element($item as item()) {
    name($item) = ("equation", "mplus", "fn", "mgroup", "factorial")
};

declare %private function common:is_inline_presentation_math_element($item as item()) {
    name($item) = ("msup", "msub", "mi", "mo", "mn")
};

declare %private function common:infix_operator_2 ($operator, $item as item()) {
    <math>
        {common:math_content_to_presentation($item/*[1])}
        <mo arity="2">{$operator}</mo>
        {common:math_content_to_presentation($item/*[2])}
    </math>
};

declare %private function common:math_content_to_presentation ($items as item()*) {
    let $arrow_function := (attribute{"class"}{"arrow_function"}, text{"&#x2192;"}) (: U+2192 rightwards arrow :)
    return
    for $item in $items return typeswitch ($item)
    case element(equation) return common:infix_operator_2("=", $item)
    case element(mplus) return common:infix_operator_2("+", $item)
    case element(mgroup) return common:math_content_to_presentation($item/node()) (: XXX :)
    case element(fn) return common:infix_operator_2($arrow_function, $item)
    case element(function) return common:infix_operator_2($arrow_function, $item)
    case element(factorial) return
        <math>{$item/*[1]}<mo>!</mo></math>
    default return
        if (common:is_inline_presentation_math_element($item))
        then $item
        else error((), "error: common:math_content_to_presentation ", trace($item, "error: common:math_content_to_presentation "))
};

declare function common:normalize ($items as item()*) {
    for $item in $items return typeswitch ($item)
    case text() return $item
    case element() return
        if (common:is_inline_presentation_math_element($item))
        then <math>{$item}</math>
        else if (common:is_content_math_element($item))
            then common:math_content_to_presentation($item)
            else element{node-name($item)}{$item/@*, common:normalize($item/node())}
    case document-node() return document { common:normalize($item/node()) } (: pitfall: document-node is not an element :)
    case comment() return $item
    default return error((), "error: common:normalize ", trace($item, "error: common:normalize "))
};

declare function common:render ($what as item()*) {
    for $child in $what return
        typeswitch ($child)
            case text() return $child
            case element(draft) | element(book) | element(chapter) | element(section) return common:render_document($child)
            default return error((), concat("Don't know how to render ", name(trace($child, "Offending child "))))
};

(:
The XQuery 3.0 documentation of the 'doc' function says that the $uri
may contain a fragment identifier, but it doesn't work on BaseX 8.6.7.
:)
declare function common:resolve ($node as node()) {
    let $url := common:get_reference_uri($node)
    let $path := substring-before($url, "#")
    let $id := substring-after($url, "#")
    let $doc := if ($path = "") then root($node) else doc($path)
    return if ($id = "") then $doc
    else id($id, $doc)
};

declare %private function common:get_reference_uri ($node as element()) {
    typeswitch ($node)
    case element(graft) return string($node/@from)
    case element(refer) return string($node/@to)
    default return error((), trace($node, "common:get_reference_uri: invalid argument, expecting refer or graft: "))
};

declare %private function common:format_resource_as_html ($resource as node()) {
    let $url := $resource/url[1]/string()
    let $title := ($resource/title/string(), $url)[1]
    let $unsafe := ($resource//unsafe)[1]
    return
    <div class="resource">
        <div class="head">Resource</div>
        <div class="body">
            <div><a href="{$url}">{$title}</a></div>
            {$unsafe/<div>Unsafe</div>}
            {$resource/description/<div class="description">{./string()}</div>}
            <div class="tags">Tags: {string-join($resource/tag, ", ")}</div>
            {for $url in $resource/url[position() > 1]/string()
                return <div>Other URL: <a href="{$url}">{$url}</a></div>}
        </div>
    </div>
};

(: deprecated :)
declare %private function common:tex_paragraph ($text as text()) as node()* {
    for $para in tokenize(string($text), "[\r\n][\r\n]") return <p>{$para}</p>
};

declare function common:render_block ($nodes as item()*) {
    for $node in $nodes return
        typeswitch ($node)
            (: case text() return common:tex_paragraph($node) :) (: deprecated because this inserts p in th and td :)
            case element(body) return <div class="body">{common:render_block($node/node())}</div>
            case element(blockquote) return <blockquote>{common:render_block($node/node())}</blockquote>
            case element(paragraph) return common:render_paragraph($node/node())
            case element(chapter) | element(section) return common:render_document($node)
            case element(code_block) return <pre>{common:render_inline($node/node())}</pre>
            case element(collection) | element(sequence) return
                element{(
                <entry key="collection" value="ul"/>,
                <entry key="sequence" value="ol"/>
                )[@key=name($node)]/@value}{
                    $node/item/<li>{common:render_inline(./node())}</li>
                }
            case element(exercises) return
                <div class="exercises">
                    {common:render_block($node/exercise)}
                </div>
            case element(exercise) return
                <div class="exercise">
                    <div class="problem">
                        <span class="label">Exercise</span>
                        {common:render_block($node/problem/node())}
                    </div>
                    <div class="answer">
                        <span class="label">Answer</span>
                        {common:render_block($node/answer/node())}
                    </div>
                </div>
            case element(table) return <table>{common:render_block($node/node())}</table>
            case element(tr) return <tr>{common:render_block($node/node())}</tr>
            case element(th) return <th>{common:render_block($node/node())}</th>
            case element(td) return <td>{common:render_block($node/node())}</td>
            case element(caption) return <caption>{common:render_inline($node/node())}</caption>
            default return common:render_inline($node)
};

declare %private function common:render_paragraph ($nodes as node()*) {
    <p>
        {
            if ($nodes/self::title)
            then attribute{"class"}{"has_title"}
            else ()
        }
        {
            for $node in $nodes return typeswitch ($node)
            case element(title) return <span class="title">{common:render_inline($node/node())}</span>
            default return common:render_inline($node)
        }
    </p>
};

declare function common:render_inline ($nodes as node()*) {
    let $templates := (
        <shell><code class="shell"><_content/></code></shell>
        , <variable><span class="variable"><_content/></span></variable>
        , <emphasis><em><_content/></em></emphasis>
        , <enquote>&#x201C;<_content/>&#x201D;</enquote>(: U+201C and U+201D are left and right double quotation mark :)
        , <omitted><span class="omitted">[...]</span></omitted>
        , <species><span class="species"><_content/></span></species>
        , <code><code><_content/></code></code>
    )
    let $recur := function ($recur, $source as node()*, $template as node()*) {
        for $t in $template/node() return
            typeswitch ($t)
                case element(_content) return common:render_inline($source/node())
                case element() return element{node-name($t)}{
                    $t/attribute(),
                    $recur($recur, $source, $t)
                }
                default return $t
    }
    return for $node in $nodes return
        let $match := $templates[name(.) = name($node)][1]
        return if ($match) then $recur($recur, $node, $match)
        else typeswitch ($node)
            case text() return $node
            case element(meta) return ()
            case element(refer) return common:render_reference($node)
            case element(link) return common:render_link($node)
            (: HTML+CSS math :)
            case element(math) return common:render_math($node)
            case element(latex) return <span class="latex_unsupported">{$node/node()}</span>
            case element(cite) return <span>FIXME CITE {$node}</span> (: FIXME :)
            default return
                let $_0 := trace($node, "render_inline: Offending node ")
                let $_1 := trace($node/.., "render_inline: Parent of offending node: ")
                let $_2 := trace($node/../.., "render_inline: Grandparent of offending node: ")
                return error((), concat("Don't know how to render ", name($node)), $node)
};

declare %private function common:render_link ($node as element(link)) {
    let $url := ($node/@url, string($node))[1]
    return <a href="{$url}">{common:render_inline($node/node())}</a>
};

declare %private function common:render_math ($nodes as item()*) {
    for $node in $nodes return typeswitch ($node)
    case element(math) return <span class="math">{common:render_math($node/node())}</span>
    case element(mi) return <span class="math identifier">{$node/node()}</span>
    case element(mo) return <span class="math operator arity_{$node/@arity} {$node/@class}">{common:render_mo_content($node/node())}</span>
    case element(mn) return <span class="math number">{$node/node()}</span>
    case element(msup) return (common:render_math($node/*[1]), <sup>{common:render_math($node/*[2])}</sup>)
    case element(msub) return (common:render_math($node/*[1]), <sub>{common:render_math($node/*[2])}</sub>)
    default return error((), concat("Don't know how to render ", name(trace($node, "render_math: Offending node "))), $node)
};

declare %private function common:render_mo_content ($nodes as item()*) {
    for $node in $nodes return typeswitch ($node)
    case text() return $node
    case element(to) return text{"&#x2192;"}
    default return error((), concat("Don't know how to render ", name(trace($node, "render_mo_content: Offending node "))), $node)
};

declare %private function common:render_reference ($reference as element()) {
    let $thing := common:resolve($reference)
    return typeswitch ($thing)
        case element(resource) return common:format_resource_as_html($thing)
        case element(draft) return
            <span>(Reference to draft: <a href="#{$thing/@id}">{common:render_inline(common:document_title_node($thing)/node())}</a>)</span>
        default return error((), string(
                let $uri := common:get_reference_uri($reference)
                let $node_name := node-name($thing)
                return <span class="internal-error">Rendering error:
                    the URI "{$uri}" resolves to a node named "{$node_name}"
                </span>))
};

declare %private function common:document_title_node ($document as element()) {
    (
        $document/title
        , $document/meta/title
        , $document/@id/<title>{replace(., "_", " ")}</title>
        , <title>(Untitled)</title>
    )[1]
};

declare %private function common:render_skills ($skills as item()*) {
    <ul>
        {
            for $skill in $skills return
                <li>{common:render_inline($skill/name/node())}</li>
        }
    </ul>
};

declare %private function common:render_document ($document as element()) {
    let $title := common:document_title_node($document)
    let $publication := $document/meta/publication
    let $is_draft := (name($document) eq "draft") or not($publication) (: TODO consider ancestor publication :)
    let $draft_indicator := if ($is_draft) then
            <div class="draft_indicator">
                <span class="title">Draft</span>
                <span>This document should not be considered published.</span>
            </div>
        else ()
    let $type := name($document)
    let $id := $document/@id
    let $toc_lines := common:table_of_contents((), 1, $document)
    let $show_toc := count($toc_lines) gt 1
    let $assumes := $document/assume/skill
    let $teaches := $document/teach/skill
    return
    <div class="document {$type}">
        {$id}
        {$draft_indicator}
        <div class="meta">
            <div class="title">About this document</div>
            <div class="body">
                <table>
                    <tr><th>Type</th><td>{$type}</td></tr>
                    <tr><th>Creation time</th><td>{$document/meta/creation-time/node()}</td></tr>
                    <tr><th>Tags</th><td></td></tr>
                    <tr><th>Parent</th><td></td></tr>
                    <tr><th>Previous sibling</th><td></td></tr>
                    <tr><th>Next sibling</th><td></td></tr>
                    <tr><th>First child</th><td></td></tr>
                    <tr><th>Teaches</th><td>{common:render_skills($teaches)}</td></tr>
                    <tr><th>Assumes</th><td>{common:render_skills($assumes)}</td></tr>
                </table>
            </div>
        </div>
        <div class="content">
            <h2 class="title">{common:render_inline($title/node())}</h2>
            {if ($show_toc) then common:render_toc_lines($toc_lines) else ()}
            {common:render_block($document/body)}
        </div>
    </div>
};

declare %private function common:render_toc_lines ($lines as element()*) {
    <div class="table_of_contents toc">
        <div class="title">Table of contents</div>
        {
            for $line in $lines return
                <div class="line">
                    <span class="number">{$line/number/node()}</span>
                    <span class="title">{common:render_inline($line/title/node())}</span>
                </div>
        }
    </div>
};

declare %private function common:table_of_contents ($prefix, $number,  $root as element()) {
    for $section in $root/body/(book | part | chapter | section) return
        let $numbered := ($section/meta/numbered/boolean(.), true())[1]
        let $complete := string-join(($prefix, $number), ".")
        return
        (
            <line>
                {if ($numbered) then <number>{$complete}</number> else ()}
                {$section/title}
            </line>
            , common:table_of_contents($complete, 1, $section)
        )
};
