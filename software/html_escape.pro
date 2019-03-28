%%  code_html(+Code:integer, -Html:string) is det.
%   Html is the result of escaping the character whose code is Code.

code_html(0'&, "&amp;") :- !.
code_html(0'<, "&lt;") :- !.
code_html(0'>, "&gt;") :- !.
code_html(0'", "&quot;") :- !.
code_html(C, H) :- string_codes(H, [C]).

%%  codes_html(+Codes:list, -Html:string) is det.
%   Html is the result of escaping the string whose codes are Codes.

codes_html([], "") :- !.

codes_html([A|B], H) :- var(H), !,
    code_html(A, HA),
    codes_html(B, HB),
    string_concat(HA, HB, H).

codes_html([A|B], H) :- string(H), !,
    string_concat(HA, HB, H),
    HA \= "",
    code_html(A, HA),
    codes_html(B, HB).

%%  string_html(String:string, Html:string) is det.
%   Html is HTML-escaped String.

string_html(S, H) :- string_codes(S, Cs), codes_html(Cs, H).
