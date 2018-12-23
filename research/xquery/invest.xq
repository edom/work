(:
./tool/basex/bin/basex -w -i invest-workflow.xml xquery/invest.xq > out/invest.html
:)
xquery version "3.0";
import module namespace common = "com.spacetimecat.xquery.common" at "common.xq";
declare copy-namespaces no-preserve, no-inherit;
declare boundary-space preserve;
declare option output:method "html";
declare option output:indent "no";

declare function local:format_price ($price as element()) {
    $price/currency || " " || format-number($price/magnitude, "###,###")
};

declare function local:format_price_attr ($price as element()) {
    $price/@currency || " " || format-number($price/@magnitude, "###,###")
};

declare function local:format_address ($x as element()) {
    let $rest :=
        if ($x/parent)
            then (text{", "}, local:format_address($x/parent))
            else ()
    return <span>{$x/type/string()}&#x20;<strong>{$x/value/string()}</strong>{$rest}</span>
};

declare function local:format_time_price ($tps) {
    <table class="data">
    {for $tp in $tps return
        <tr>
            <th>{$tp/@time/string()}</th>
            <td>{local:format_price($tp)}</td>
        </tr>}
    </table>
};

declare function local:format_time_price_attr ($tps) {
    local:format_time_price(for $tp in $tps return
        <element time="{$tp/@time}">
            <currency>{$tp/@currency/string()}</currency>
            <magnitude>{$tp/@magnitude/string()}</magnitude>
        </element>
    )
};

declare function local:format_time_interval_attr ($x) {
    <span>{$x/@begin/string()} – {$x/@endex/string()}</span>
};

declare function local:format_time_interval_price_attr ($tps) {
    <table class="data">
    {for $tp in $tps return
        <tr>
            <th>{local:format_time_interval_attr($tp)}</th>
            <td>{local:format_price_attr($tp)}</td>
        </tr>}
    </table>
};

declare function local:format_time_count ($tcs) {
    <table class="data">
    {for $tp in $tcs return
        <tr>
            <th>{$tp/@time/string()}</th>
            <td>{fn:format-number($tp/@value, "###,###")}</td>
        </tr>}
    </table>
};

declare function local:format_stock ($stock as element()) {
    let $exchange := $stock/exchange/string()
    let $ticker := $stock/ticker/string()
    let $numbers := $stock/numbers
    let $market_caps := $numbers/market_cap
    let $share_prices := $numbers/share_price
    let $share_counts := $numbers/share_count
    return
    <div class="stock">
        <table>
            <tr><th>Exchange</th><td>{$exchange}</td></tr>
            <tr><th>Ticker</th><td>{$ticker}</td></tr>
            {if (empty($market_caps)) then () else
                <tr>
                    <th>Market capitalization</th>
                    <td>{local:format_time_price($market_caps)}</td>
                </tr>}
            <tr>
                <th>Share price</th>
                <td>{local:format_time_price($share_prices)}</td>
            </tr>
            <tr>
                <th>Share count</th>
                <td>{local:format_time_count($share_counts)}</td>
            </tr>
        </table>
    </div>
};

declare function local:format_asset ($asset as element()) {
    let $name := $asset/name/string()
    let $liquidation := $asset/appraisal/liquidation
    let $book_value := $asset/appraisal/book_value
    let $address := $asset/address
    return
        <div class="asset">
            <h3>{$name}</h3>
            <table>
                <tr><th>Name</th><td>{$name}</td></tr>
                {if (empty($liquidation)) then () else
                <tr>
                    <th>Liquidation price</th>
                    <td>
                        <div>{local:format_price($liquidation/price)}</div>
                        {common:render_block($liquidation/reason/node())}
                    </td>
                </tr>}
                <tr>
                    <th>Book value</th>
                    <td>{local:format_time_price_attr($book_value)}</td>
                </tr>
                <tr>
                    <th>Build date</th>
                    <td>{$asset/build_date/string()}</td>
                </tr>
                <tr>
                    <th>Gross cash flow income</th>
                    <td>{for $income in $asset/income return
                        <div>
                            <div>{$income/description/string()}</div>
                            <div>{local:format_time_interval_price_attr($income/sample)}</div>
                        </div>}
                    </td>
                </tr>
                <tr>
                    <th>Expenses</th>
                    <td>{for $expense in $asset/expense return
                        <div>
                            <div>{$expense/description/string()}</div>
                            <div>{local:format_time_interval_price_attr($expense/sample)}</div>
                        </div>}
                    </td>
                </tr>
                <tr>
                    <th>Area</th>
                    <td>{$asset/area/string()} square meters</td>
                </tr>
                {if (empty($address)) then () else
                    <tr>
                        <th>Address</th>
                        <td>{local:format_address($asset/address)}</td>
                    </tr>}
            </table>
        </div>
};

declare function local:format_date ($date as xs:date) {
    let $year_count := year-from-date(current-date()) - year-from-date($date)
    return <span>{$date} ({$year_count} years ago)</span>
};

declare function local:format_company ($company as element()) {
    let $basic := $company/basic
    let $stock := $company/stock
    let $name := $basic/name/string()
    let $established := $basic/established/xs:date(.)
    let $date_ipo := $basic/date_ipo/xs:date(.)
    let $assets := $company/asset
    let $problems := $company/problem
    let $news := $company/news
    let $liquidations := $assets/appraisal/liquidation
    let $verdicts := $company//verdict
    let $liqudation :=
        <liquidation>
            <price>
                <currency>IDR</currency>
                <magnitude>{sum($liquidations/price/magnitude/xs:decimal(.))}</magnitude>
            </price>
        </liquidation>
    return
    <div class="company">
        <h1 class="name">{$name}</h1>
        <table>
            <tr><th>Name</th><td>{$name}</td></tr>
            {if (empty($established)) then () else
                <tr><th>Established</th><td>{local:format_date($established)}</td></tr>
            }
            {if (empty($date_ipo)) then () else
                <tr><th>IPO date</th><td>{local:format_date($date_ipo)}</td></tr>
            }
            <tr><th>Liquidation price</th><td>{local:format_price($liqudation/price)}</td></tr>
        </table>
        {if (empty($stock)) then () else
            <div class="stock">
                <h2>Stock</h2>
                {local:format_stock($stock)}
            </div>}
        {if (empty($verdicts)) then () else
            <div>
                <h2>Verdicts</h2>
                {for $verdict in $verdicts order by $verdict/@time descending return
                    <table>
                        <tr><th>Decision</th><td>{$verdict/@type/string()}</td></tr>
                        <tr><th>Decision time</th><td>{$verdict/@time/string()}</td></tr>
                        <tr><th>Reason</th><td>{common:render_block($verdict/reason/node())}</td></tr>
                    </table>}
            </div>
        }
        <h2>Assets</h2>
        {for $asset in $assets return local:format_asset($asset)}
        {if (empty($problems)) then () else
            <div>
                <h2>Problems</h2>
                {for $problem in $problems return
                    <div>
                        <h3>{common:render_inline($problem/title/node())}</h3>
                        {common:render_block($problem/body/node())}
                    </div>}
            </div>}
        {if (empty($news)) then () else
            <div>
                <h2>News</h2>
                {for $new in $news return
                    <div>
                        <h3>{common:render_inline($new/title/node())}</h3>
                        {common:render_block($new/body/node())}
                    </div>
                }
            </div>
        }
    </div>
};

<html>
    <head>
        <title>Analysis of public companies</title>
        <link rel="stylesheet" type="text/css" href="../style.css"/>
    </head>
    <body>
        {for $company in //company return local:format_company($company)}
    </body>
</html>
