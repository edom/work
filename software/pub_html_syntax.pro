/** <module> HTML syntax

See also library(http/html_write).

tokens//1 is for lexical analysis.

content//1 is for syntactic analysis.
*/
:- module(pub_html_syntax, [
    tokens//1,
    content//1,
    ast_cst/2,
    test/0,
    test1/0
]).
:- use_module('./dcg_common.pro').

% Lexical analysis.

tokens([]) --> [].
tokens([H|T]) --> token(H), tokens(T).
token(decl(N,A)) --> tag_decl(N,A).
token(close(N,A)) --> tag_close(N,A).
token(open(N,A)) --> tag_open(N,A).
token(chars(A)) --> chars(A).
tag_decl(N,T) --> "<!", name_atom(N), intag(T), ">".
tag_open(N,T) --> "<", name_atom(N), intag(T), ">".
tag_close(N,T) --> "</", name_atom(N), intag(T), ">".
intag([]) --> [].
intag([H|T]) --> [H], {H \= 0'>}, intag(T).
chars([H|T]) --> char(H), chars(T).
chars([H]) --> char(H).
char(H) --> [H], {H \= 0'<}.

% Tag name, attribute name, ID.
name([H|T]) --> namechar(H), name(T).
name([H]) --> namechar(H).

name_atom(A) --> {when((ground(A);ground(C)), atom_codes(A,C))}, name(C).

namechar(H) --> [H], {code_type(H,csym);H=0'-}.

test :-
    string_codes("<html dafuq=\"51\">foo<body>bar</body></html>",C),
    %string_codes("<html dafuq=51>foo</html>",C),
    phrase(tokens(T), C, []), !,
    print(T), nl,
    phrase(content(K), T, []), !,
    print(K), nl.

test1 :-
    AK = [e(html,[a(foo,bar), a(baz,qux)],[chars(foobarbazqux)])],
    print(AK), nl,
    ast_cst(AK,K),
    print(K), nl,
    phrase(content(K), T, []), !,
    print(T), nl,
    phrase(tokens(T), C, []), !,
    string_codes(S,C),
    write(S), nl.

ast_cst([], []).
ast_cst([AH|AT], [CH|CT]) :- astnode_cstnode(AH,CH), ast_cst(AT,CT).

astnode_cstnode(e(N,AAttrs,AContent), e(N,CAttrs,CContent)) :-
    astattr_cstattr(AAttrs, CAttrs),
    ast_cst(AContent, CContent).
astnode_cstnode(chars(A),chars(C)) :- avalue_cvalue(A,C).

astattr_cstattr([],[]).
astattr_cstattr([a(N,AV)|AT],[s([32]),a(N,CV)|CT]) :- avalue_cvalue(AV,CV), astattr_cstattr(AT,CT).

avalue_cvalue(A,C) :- string(A), !, string_codes(A,C).
avalue_cvalue(A,C) :- atom(A), !, atom_codes(A,C).
avalue_cvalue(A,C) :- is_list(A), !, A=C.

% Syntactic analysis.

elem(Name,Attrs,Content) -->
    [open(Name,CAttrs)],
    {phrase(attrs(Attrs),CAttrs,[])},
    content(Content),
    [close(Name,[])].

content([]) --> [].
content([e(N,A,C)|T]) --> elem(N,A,C), content(T).
content([H|T]) --> [H], content(T).

attrs([]) --> [].
attrs([s(S),a(N,V)|T]) --> spaces1(S), attr(N,V), attrs(T).
attrs([s(S)]) --> spaces1(S).

attr(Name,Value) --> name_atom(Name), "=\"", reluctant(Value), "\"".

/*
elems([]) --> [].
elems([H|T]) --> elem(H), elems(T).

tag_open(Name,Attrs) --> "<", name(Name), attrs(Attrs), ">".
tag_close(Name) --> "</", name(Tag), ">".
*/
