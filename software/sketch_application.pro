% -------------------- behavior

:- dynamic state/2.

:- op(10,fx,#).
:- op(10,xfx,@).
:- op(100,yfx,++).

interpret(A, Z) :- interpret([], A, Z).

interpret(C, A * B, Z) :- !,
    interpret(C, A, A0),
    interpret(C, B, B0),
    Z is A0 * B0.

interpret(C, A + B, Z) :- !,
    interpret(C, A, A0),
    interpret(C, B, B0),
    Z is A0 + B0.

interpret(C, (A,B), Z) :- !,
    interpret(C, A, _),
    interpret(C, B, Z).

interpret(_, states([]) := [], Z) :- !, Z = unit.
interpret(C, states([A|B]) := [X|Y], Z) :- !,
    interpret(C, state(A) := X, _),
    interpret(C, states(B) := Y, Z).

interpret(C, A ++ B, Z) :- !, interpret(C,A,A0), interpret(C,B,B0), append(A0,B0,Z).

interpret(_, quote(A), Z) :- !, Z = A.

interpret(C, let([],B), Z) :- !, interpret(C, B, Z).
interpret(C, let([Name=Exp|Binds],Body), Z) :- !,
    interpret(C, Exp, Val),
    interpret([Name-Val|C], let(Binds,Body), Z).

interpret(C, $Name, Z) :- !,
    (member(Name-Val, C)
    ->  Z = Val
    ;   existence_error(variable, Name)
    ).

interpret(C, state(S) := E, Z) :- !,
    Z = unit,
    interpret(C, E, V),
    retractall(state(S,_)),
    assertz(state(S,V)).

interpret(_, [] := [], Z) :- !, Z = unit.
interpret(C, [A|B] := [X|Y], Z) :- !,
    interpret(C, A := X, _),
    interpret(C, B := Y, Z).

interpret(_, state(S), Z) :- state(S, V), !, Z = V.
interpret(_, state(S), _) :- !, type_error(state, state(S)).
interpret(_, format(A,B), Z) :- !, Z = unit, format(A,B).
interpret(_, read, Z) :- !, read(Z).
interpret(_, [], Z) :- !, Z = [].
interpret(C, [A|B], Z) :- !, Z = [A0|B0], interpret(C, A, A0), interpret(C, B, B0).
interpret(C, context, Z) :- !, Z = C.
interpret(_, A, Z) :- number(A), !, Z = A.
interpret(_, E, _) :- !, type_error(expression, E).

% -------------------- structure

class(person).
class_property(person, name).
class_property(person, food).
class_property(person, birth_date).

% database(?Class, ?Id, ?Object) is nondet.
:- dynamic database/3.

% dummy test data
database(person, -2, [name-"Alice", food-"bread", birth_date-date(1990,1,2)]).
database(person, -1, [name-"Bob", food-"meat", birth_date-date(1990,3,4)]).

term_locale_string(property(person,name), eng, "name").
term_locale_string(property(person,food), eng, "food").
term_locale_string(property(person,birth_date), eng, "birth date").
term_locale_string(property(person,name), ind, "nama").
term_locale_string(property(person,food), ind, "makanan").
term_locale_string(property(person,birth_date), ind, "tanggal lahir").
term_locale_string(property(person,name), jpn, "名前").
term_locale_string(property(person,food), jpn, "食物").
term_locale_string(property(person,birth_date), jpn, "誕生日").

globalize(Term, Locale, String) :- term_locale_string(Term, Locale, String), !.
globalize(property(_,Name), _, String) :- !, to_string(Name, String).
globalize(Term, _, String) :- to_string(Term, String).

opv_get(Obj, Prop, Val) :-
    nonvar(Prop),
    \+ member(Prop-Val, Obj), !,
    throw(error(no_such_property(Prop),_)).

opv_get(Obj, Prop, Val) :- !,
    member(Prop-Val, Obj).

class_th(Cls, th(Str)) :-
    class_property(Cls, Prop),
    globalize(property(Cls,Prop), eng, Str).

object_td(Cls, Obj, td(Str)) :-
    class_property(Cls, Prop),
    opv_get(Obj, Prop, Val),
    to_string(Val, Str).

to_string(A, Z) :- string(A), !, Z = A.
to_string(A, Z) :- !, term_string(A, Z).

object_tr(Cls, Obj, tr(Tds)) :-
    findall(Td, object_td(Cls,Obj,Td), Tds).

objects_html_table(Cls, Objs, table(thead(tr(Ths)),tbody(Trs))) :-
    findall(Th, class_th(Cls,Th), Ths),
    findall(Tr, (member(Obj,Objs), object_tr(Cls,Obj,Tr)), Trs).

:- nb_setval(database_id, 0).

generate_id(Id) :-
    nb_getval(database_id, Id),
    Id1 is Id + 1,
    nb_setval(database_id, Id1).

insert(Class, Object) :-
    generate_id(Id),
    assertz(database(Class, Id, Object)).

read_class(Class, Object) :-
    findall(
        Prop-Val,
        read_property(Class, Prop, Val),
        Object
    ).

read_property(C, P, V) :-
    class_property(C, P),
    format("Input ~w (Prolog term, end with period and Enter):\n", P),
    read(V).

% -------------------- test

run :-
    read_class(person, A),
    insert(person, A).

% -------------------- web

:- use_module(library(http/thread_httpd), [
    http_server/2
]).
:- use_module(library(http/http_parameters), [
    http_parameters/2
]).
:- use_module(library(http/http_dispatch), [
    http_reply_file/3
]).
:- use_module(library(http/http_error), []).
:- nodebug(http(error)).
%:- set_setting(http:client_backtrace, false).
:- set_setting(http:client_backtrace, true). % DEBUG

:- consult_unregistered("html_cphe.pro").

web :- http_server(dispatch, [port(4003)]).

request_get_parameter(Request, Key, Value) :-
    member(search(Gets), Request),
    member(Key=Value, Gets).

urlpath_filepath_type('/static/style.css', 'sketch_application.css', 'text/css;charset=UTF-8').

dispatch(Request) :-
    member(method(get), Request),
    member(path(Url), Request),
    urlpath_filepath_type(Url, File, Type),
    !,
    Opts = [
        mime_type(Type),
        unsafe(true)
    ],
    http_reply_file(File, Opts, Request).

dispatch(Request) :-
    member(method(get), Request),
    member(path('/'), Request),
    findall(Obj, database(person,_,Obj), Objs),
    objects_html_table(person, Objs, Table),
    http_parameters(Request, [
        x(X,[integer,default(0)])
        , y(Y,[integer,default(0)])
    ]),
    Z is X + Y,
    Doc = [
        '!doctype'(html)
        , html(
            head(
                title("Title")
                , meta(charset="UTF-8")
                , meta(name=viewport, content="width=device-width, initial-scale=1")
                , link(rel=stylesheet, type="text/css", href="/static/style.css")
            )
            , body(
                header(
                    h1("Application")
                )
                , main(
                    Table
                    , form(method="GET", action="."
                        , div(class=form_fit
                            , div(class=form_input_table
                                , label(
                                    span(class=label_left, "Z")
                                    , span(class=control_wide, Z)
                                )
                                , label(
                                    span(class=label_left, "X"),
                                    input(class=control_wide, name=x, value=X)
                                )
                                , label(
                                    span(class=label_left, "Y"),
                                    input(class=control_wide, name=y, value=Y)
                                )
                                , label(
                                    span(class=label_left, "A long-named but unused parameter"),
                                    input(class=control_wide, name=z, value=Z)
                                )
                                , label(
                                    input(class=control_narrow, type=checkbox)
                                    , span(class=label_right, "a test checkbox")
                                )
                            )
                            , input(type=submit)
                        )
                    )
                )
            )
        )
    ],
    cphe_string(Doc, String),
    format('Content-Type: text/html; charset=UTF-8~n'),
    format('~n'),
    write(String).


