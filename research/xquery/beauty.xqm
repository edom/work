xquery version "3.0";

import module namespace common = "com.spacetimecat.xquery.common" at "common.xq";

(:
$node is the xi:include element.
This is non-recursive.

Workaround for BaseX 7.
BaseX 8 processes XInclude.

Deprecated. Use resolve_graft_or_refer instead.
:)
declare function local:resolve_xinclude ($node as node()) {
    let $path := ($node/@href/string(), "")[1]
    let $id := substring-after($node/@xpointer/string(), "#")
    let $doc := if ($path = "") then root($node) else doc($path)
    return if ($id = "") then $doc
    else id($id, $doc)
};

declare function local:resolve ($node as node()) {
    common:resolve($node)
};

declare function local:main_beauty ($root) {
    for $beauty in $root//beauty
    return
        let $subject_or_refer := $beauty/subject[1]/*[1]
        let $subject := local:resolve($subject_or_refer)
        let $name := ($subject/name[1]/string(), "(Name unknown)")[1]
        let $fapworthy := $beauty//fapworthy[1]
        let $unsafe := ($beauty//unsafe, $subject//unsafe)[1]
        (: merge into 'resource' element with 'type' subelement? :)
        let $resources := ($subject/website, $beauty/reason/(website|image|video|resource))
        return
            <html>
                <head>
                    <link rel="stylesheet" href="../style.css"/>
                </head>
                <body>
                    <div class="beauty">
                        <h3 class="name">{$name}</h3>
                        {$unsafe/<div>Unsafe</div>}
                        {$fapworthy/<div>Fapworthy</div>}
                        {$subject/fictional[1]/<div>Fictional character</div>}
                        {$subject/description/<div>{./string()}</div>}
                        <div class="resources">
                            {$resources/common:format_resource_as_html(.)}
                        </div>
                    </div>
                </body>
            </html>
};

declare function local:main_learn () {
};

local:main_beauty(/)
