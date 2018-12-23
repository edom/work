(: generate website from index.xml :)

xquery version "3.0";

import module namespace common = "com.spacetimecat.xquery.common" at "common.xq";

declare option output:method "html";
declare option output:indent "no"; (: indenting causes unwanted leading space in HTML spans :)

declare variable $output_dir := "out";

declare %private function common:make_site ($site as element(site)) {
    for $hf in $site/html_file return common:make_html_file($hf)
};

declare %private function common:make_html_file ($hf as element(html_file)) {
    $output_dir
};

declare function common:render_skill ($skills as element(skill)*) {
    for $skill in $skills return
        let $assumers := root($skill)//assume[skill = $skill]
        let $teachers := root($skill)//teach[skill = $skill]
        let $needs := $skill/need/skill
        return
        <div class="skill">
            <h2 class="name">{common:render_inline($skill/name/node())}</h2>
            {
                if ($teachers)
                then <div class="teachers">
                    <h4>Taught by</h4>
                    <ul>
                        {
                            for $teacher in $teachers return
                                <li>{common:render_inline($teacher/../title/node())}</li>
                        }
                    </ul>
                </div>
                else ()
            }
            {
                if ($needs)
                then <div class="needs">
                    <h4>Needs</h4>
                    <ul>
                        {
                            for $need in $needs return
                                <li>{common:render_inline($need/name/node())}</li>
                        }
                    </ul>
                </div>
                else ()
            }
            {
                if ($assumers)
                then <div class="assumers">
                    <h4>Assumed by</h4>
                    <ul>
                        {
                            for $assumer in $assumers return
                                <li>{common:render_inline($assumer/../title/node())}</li>
                        }
                    </ul>
                </div>
                else ()
            }
        </div>
};

declare %private function local:content ($normal, $site, $html_file) {
    <html>
        <head>
            <title>{common:render_inline($html_file/title/node())}</title>
            <meta charset="UTF-8"/>
            <meta name="viewport" content="width=device-width, initial-scale=1"/>
            <link rel="stylesheet" type="text/css" href="../style.css"/>
        </head>
        <body>
            {
                for $con in $html_file/content/node() return typeswitch ($con)
                case text() return $con
                case element(index) return
                    <div>
                        <h1>Index</h1>
                        {
                            for $file in $site/html_file
                            let $path := $file/path/string()
                            return
                                <div><a href="{$path}">{$path}</a></div>
                        }
                    </div>
                case element(graft) return common:render(common:resolve($con))
                case element(skills) return
                    <div>
                        <h1>Skills</h1>
                        {common:render_skill($normal//skill)}
                    </div>
                case element(documents) return
                    let $documents := $normal//(book | draft) return
                    <div>
                        <h1>Documents</h1>
                        {common:render($documents)}
                    </div>
                default return error((), concat("content: Don't know how to render ", name($con)), $con)
            }
        </body>
    </html>
};

let $normal := common:normalize(/)
let $site := $normal//site[1]
let $sp_html :=
    <output:serialization-parameters>
        <output:method value="html"/>
        <output:version value="5.0"/>
        <output:encoding value="UTF-8"/>
        <output:indent value="no"/>
    </output:serialization-parameters>
for $html_file in $site/html_file
let $rel_path := $html_file/path/string()
let $path := string-join(($output_dir, $rel_path), "/")
let $output := local:content($normal, $site, $html_file)
return
    file:write-text(
        $path
        , serialize($output, $sp_html)
        , "UTF-8"
    )
