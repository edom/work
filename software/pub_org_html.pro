:- module(pub_org_html, [
    test/0
]).
:- use_module('./dcg_stateful.pro').
:- use_module('./pub_html_syntax.pro').
:- use_module('./pub_org_syntax.pro').
:- use_module('./pub_org_process.pro').

test :-
    string_codes("Hi. *foo*[fn::foo][fn::bar]", Codes0),
    phrase_stateful(pub_org_syntax:document(D), Codes0, []), !, print(d=D),nl,
    process_footnotes([],D,Footnotes,DF), print(df=DF),nl, print(footnotes=Footnotes),nl,
    orgast_htmlast(DF,HA), print(ha=HA),nl,
    pub_html_syntax:ast_cst(HA,HC), print(hc=HC),nl,
    phrase(pub_html_syntax:content(HC), HT, []), !, print(ht=HT),nl,
    phrase(pub_html_syntax:tokens(HT), Codes1, []), !,
    string_codes(HS,Codes1),
    write(HS),nl.

orgast_htmlast([],[]).
orgast_htmlast([OH|OT],[HH|HT]) :- orgnode_htmlnode(OH,HH), orgast_htmlast(OT,HT).

orgnode_htmlnode(paragraph(O), e(p, [], H)) :- orgast_htmlast(O,H).
orgnode_htmlnode(s(C), chars(C)).
orgnode_htmlnode(fnref(N), e(sup, [], [e(a, [a(href,CHref)], [chars(C)])])) :-
    term_string(N,S), string_codes(S,C0), append([[ 0'[ ],C0,[ 0'] ]],C),
    string_concat("#fn", N, Href),
    string_codes(Href,CHref).
