:- consult_unregistered("html_escape.pro").

% -------------------- public

cphe_codes(Exp, Codes) :-
    cphe_ast(Exp, Ast),
    (phrase(cphe_dcg(Ast), Codes)
    ->  true
    ;   type_error(cphe_dcg_ast, Ast)).

cphe_string(Exp, String) :-
    cphe_codes(Exp, Codes),
    string_codes(String, Codes).

% -------------------- private

:- consult_unregistered("html_tag.pro").

cphe_is_attr(Exp) :- cphe_attr_name_value(Exp, _, _).
cphe_is_elem(Exp) :- functor(Exp, Name, _), html_tag(Name).

cphe_attr_name_value(K=V, K, V).

cphe_elem_name(Exp, Name) :-
    cphe_is_elem(Exp),
    functor(Exp, Name, _).

cphe_elem_attr(Exp, K, V) :-
    arg(_, Exp, Attr),
    cphe_attr_name_value(Attr, K, V).

cphe_elem_child(Exp, C) :-
    arg(_, Exp, C),
    \+ cphe_is_attr(C).

cphe_ast([], Z) :- !,
    cphe_ast_list([], Z).

cphe_ast([A|B], Z) :- !,
    cphe_ast_list([A|B], Z).

cphe_ast('!doctype'(D), Z) :- !,
    Z = doctype(D).

cphe_ast(Exp, Z) :-
    cphe_elem_name(Exp, Name), !,
    Z = elem(Name,Attrs,Children),
    findall(attr(K,V), cphe_elem_attr(Exp,K,V), Attrs),
    findall(Child, (cphe_elem_child(Exp,C), cphe_ast(C,Child)), Children).

cphe_ast(Exp, Z) :-
    string(Exp), !,
    Z = text(Exp).

cphe_ast(Exp, Z) :-
    number(Exp), !,
    number_string(Exp, Text),
    Z = text(Text).

cphe_ast(Exp, _) :- !,
    functor(Exp, Name, _),
    type_error(cphe_html_tag, Name).

cphe_ast_list([], Z) :- !, Z = [].
cphe_ast_list([A|B], Z) :- !,
    Z = [A0|B0],
    cphe_ast(A, A0),
    cphe_ast_list(B, B0).

cphe_dcg([]) --> !.
cphe_dcg([A|B]) --> !, cphe_dcg(A), cphe_dcg(B).
cphe_dcg(doctype(A)) --> !, "<!doctype ", cphe_dcg(text(A)), ">".
cphe_dcg(text(A)) --> !, {string_html(A,Str)}, Str.
cphe_dcg(attr(K,V)) --> !, " ", cphe_dcg(text(K)), "=""", cphe_dcg(text(V)), """".
cphe_dcg(elem(Name,Attrs,Children)) --> !,
    "<", cphe_dcg(text(Name)), cphe_dcg(Attrs), ">",
    cphe_dcg(Children),
    ({html_empty_tag(Name)} -> [] ; "</", cphe_dcg(text(Name)), ">").
cphe_dcg_(A) --> !,
    {type_error(cphe_ast, A)}.
