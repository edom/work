xquery version "1.0";
declare namespace local = "test.xquery#local";
declare namespace xs = "http://www.w3.org/2001/XMLSchema";

declare function local:apply ($grammar, $rules, $input as xs:string) {
    let $head := $rules[1]
    let $tail := $rules[position() > 1]
    return
        if (empty($rules)) then <ok remaining="{$input}"/>
        else typeswitch (local:step($grammar, trace($head, "apply.head"), $input))
            case $r_head as element(ok) return
                typeswitch (local:apply($grammar, $tail, $r_head/@remaining))
                case $r_tail as element(ok) return
                    <ok remaining="{$r_tail/@remaining}">
                        {$r_head/*}
                        {$r_tail/*}
                    </ok>
                default $e return $e
            default $e return $e
};

declare function local:step ($grammar, $rule, $input as xs:string) {
    typeswitch ($rule)

        case element(end) return
            if (string-length($input) = 0) then <ok remaining=""/>
            else <error remaining="{$input}"><rule>{$rule}</rule></error>

        case element(fixed) return
            let $expected := $rule/@string
            let $remaining := substring($input, string-length($expected) + 1)
            return if (starts-with($input, $expected))
            then<ok remaining="{$remaining}">
                    {$rule}
                </ok>
            else <error remaining="{$input}"><rule>{$rule}</rule></error>

        case element(refer) return
            let $result := local:apply($grammar, $grammar//*[@name = $rule/@to], $input)
            return typeswitch($result)
            case element(ok) return
                <ok remaining="{$result/@remaining}">
                    {$result/*}
                </ok>
            default return $result

        case element(char) return
            let $actual := substring($input, 1, 1)
            return
            if (string-length($actual) gt 0 and contains($rule/@in, $actual))
            then <ok remaining="{substring($input, 2)}">
                <char value="{$actual}"/>
            </ok>
            else <error remaining="{$input}"><rule>{$rule}</rule></error>

        case element(optional) return
            typeswitch (local:apply($grammar, $rule/*, $input))
                case $result as element(ok) return $result
                default return <ok remaining="{$input}"/>

        case element(choice) return
            let $choices := $rule/*
            let $alternative := <choice>{$choices[position() > 1]}</choice>
            return if (empty($choices)) then
                <error remaining="{$input}"><rule>{$rule}</rule></error>
            else typeswitch (local:apply($grammar, $choices[1], $input))
                case $result as element(ok) return $result
                default return local:apply($grammar, $alternative, $input)

        case element(sequence) return
            let $result := local:apply($grammar, $rule/*, $input)
            return typeswitch($result)
                case element(ok) return
                    <ok remaining="{$result/@remaining}">
                        <sequence>
                            {$rule/@name}
                            {$result/*}
                        </sequence>
                    </ok>
                default return $result

        default return <error remaining="{$input}"><rule>{$rule}</rule></error>
};

<outputs>
{
    for $app in /test/apply
    let $grammar := id($app/@grammar)
    let $input := $app/@input
    let $top := $grammar//*[@name = "top"][1]
    return local:apply($grammar, trace($top, "top"), $input)
}
</outputs>
