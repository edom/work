% -------------------- input

% structure
:- multifile class/1.
:- multifile class_property/2.
:- multifile class_property_type/3.

% globalization
:- multifile globalization_locale/1.
:- multifile term_locale_string/3.

% static file serving
:- multifile urlpath_filepath_type/3.

% -------------------- operation

start_http_server :- http_server(dispatch, [port(4003)]).

% -------------------- private

% -------------------- linking

:- import(file("enterprise/library/sql.pro"),[
    multifiles([
        class/1
        , class_property/2
        , class_property_type/3
    ])
]).

% -------------------- syntax

:- op(10,fx,#).
:- op(10,xfx,@).
:- op(100,yfx,++).

% -------------------- globalization

request_locale(_, eng).

cphe_globalize(L, #globalize(A), Z) :- !, term_loc_str(A, L, Z).
cphe_globalize(L, A=B, Z) :- !, Z = (A=B0), cphe_globalize(L, B, B0).

cphe_globalize(_, [], Z) :- !, Z = [].
cphe_globalize(L, [A|B], Z) :- !,
    Z = [A0|B0],
    cphe_globalize(L, A, A0),
    cphe_globalize(L, B, B0).

cphe_globalize(_, A, Z) :- string(A), !, Z = A.

cphe_globalize(L, A, Z) :-
    A =.. [Name|Args],
    cphe_globalize(L, Args, Args0),
    Z =.. [Name|Args0].

% -------------------- state

:- dynamic state/2.

% database(?Class, ?Id, ?Object) is nondet.
:- dynamic database/3.
:- multifile database/3.

% -------------------- behavior

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

term_loc_str(Term, Locale, String) :- term_locale_string(Term, Locale, String), !.
term_loc_str(class(Name), _, String) :- !, to_string(Name, String).
term_loc_str(property(_,Name), _, String) :- !, to_string(Name, String).
term_loc_str(Term, _, String) :- to_string(Term, String).

% -------------------- derive HTML table

opv_get(Obj, Prop, Val) :-
    nonvar(Prop),
    \+ member(Prop-Val, Obj), !,
    throw(error(no_such_property(Prop),_)).

opv_get(Obj, Prop, Val) :- !,
    member(Prop-Val, Obj).

class_th(Cls, th(#globalize(property(Cls,Prop)))) :-
    class_property(Cls, Prop).

object_td(Cls, Obj, td(Str)) :-
    class_property(Cls, Prop),
    opv_get(Obj, Prop, Val),
    to_string(Val, Str).

% For display to humans, not for fidelity/storage/readback.
to_string(A, Z) :- string(A), !, Z = A.
to_string(A, Z) :- !, term_string(A, Z).

object_tr(Cls, Obj, tr(Tds)) :-
    findall(Td, object_td(Cls,Obj,Td), Tds).

objects_html_table(Cls, Objs, table(thead(tr(Ths)),tbody(Trs))) :-
    findall(Th, class_th(Cls,Th), Ths),
    findall(Tr, (member(Obj,Objs), object_tr(Cls,Obj,Tr)), Trs).

% -------------------- derive HTML form for insert

class_html_create_input(Cls, Html) :-
    Html = label(
        span(class=label_left, #globalize(property(Cls,Prop)))
        , input(class=control_wide, type=text, name=Prop)
    ),
    class_property(Cls, Prop).

class_html_create_form(Cls, Html) :-
    urlexp_compute(create(Cls), Url),
    Html = form(method="POST", action=Url
        , div(class=form_title, "Create ", #globalize(class(Cls)))
        , div(class=form_fit
            , div(class=form_input_table, Inputs)
            , input(type=submit, value="Create")
        )
    ),
    findall(Input, class_html_create_input(Cls,Input), Inputs).

% -------------------- internal linking

urlexp_compute(create(Cls), Url) :- !,
    atomic_list_concat([/,Cls,'/create'], Url).

urlexp_compute(Exp, _) :-
    type_error(url_expression, Exp).

% -------------------- global variables

% This does not seem thread-safe.

:- dynamic globalvar/2.

global_setval(Name, Val) :-
    retractall(globalvar(Name,_)),
    assertz(globalvar(Name,Val)).

global_getval(Name, _) :- \+ globalvar(Name,_), !, existence_error(global_var, Name).
global_getval(Name, Val) :- globalvar(Name, Val).

% -------------------- database?

:- global_setval(database_id, 0).

database_generate_id(Id) :-
    global_getval(database_id, Id),
    Id1 is Id + 1,
    global_setval(database_id, Id1).

database_insert(Class, Object) :-
    database_generate_id(Id),
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

% -------------------- web

:- use_module(library(http/thread_httpd), [
    http_server/2
]).
:- use_module(library(http/http_parameters), [
    http_parameters/2
    , http_parameters/3
]).
:- use_module(library(http/http_dispatch), [
    http_reply_file/3
]).
:- use_module(library(http/http_error), []).
:- nodebug(http(error)).
%:- set_setting(http:client_backtrace, false).
:- set_setting(http:client_backtrace, true). % DEBUG

:- import(file("html_cphe.pro"),[
]).

request_parameter_get(Request, Key, Value) :-
    member(search(Gets), Request),
    member(Key=Value, Gets).

:- use_module(library(debug), [
    debug/3
]).

% -------------------- page expression

pageexp_interpret_0(Req, Exp) :-
    request_locale(Req, Loc),
    pageexp_interpret(Exp, Glo),
    cphe_globalize(Loc, Glo, Doc),
    cphe_string(Doc, String),
    format('Content-Type: text/html; charset=UTF-8~n'),
    format('~n'),
    write(String).

% Template.
pageexp_interpret(Exp, Htm) :-
    Htm = [
        '!doctype'(html)
        , html(
            head(
                title(Title)
                , meta(charset="UTF-8")
                , meta(name=viewport, content="width=device-width, initial-scale=1")
                , link(rel=stylesheet, type="text/css", href="/static/style.css")
            )
            , body(
                header(
                    h1("Example web application")
                )
                , main(Body)
            )
        )

    ],
    pageexp_interpret([], S, Exp, Body),
    (member(title-Title, S) -> true ; S = "Untitled").

pageexp_interpret(S0, S9, (A,B), Z) :- !,
    pageexp_interpret(S0, S1, A, _),
    pageexp_interpret(S1, S9, B, Z).

pageexp_interpret(S0, S9, (Var := Exp), Z) :- !,
    S9 = [Var-Z|S1],
    pageexp_interpret(S0, S1, Exp, Z).

pageexp_interpret(S0, S9, A+B, Z) :- !,
    pageexp_interpret(S0, S1, A, A0),
    pageexp_interpret(S1, S9, B, B0),
    string_concat(A0, B0, Z).

pageexp_interpret(S0, S9, Exp, Z) :- Exp =.. [html|Args], !, S0 = S9, Z = Args.
pageexp_interpret(S0, S9, Exp, Z) :- string(Exp), !, S0 = S9, Z = Exp.
pageexp_interpret(S0, S9, Exp, Z) :- atom(Exp), !, S0 = S9, atom_string(Exp, Z).

pageexp_interpret(_, _, Exp, _) :- !,
    type_error(page_expression, Exp).

% -------------------- dispatch

formdata_objprop(A=B, Z) :- !, Z = A-B.
formdata_objprop(A, _) :- type_error(form_data, A).

database_view(Cls, Table) :-
    findall(Obj, database(Cls,_,Obj), Objs),
    objects_html_table(Cls, Objs, Table).

% Serve static files.
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
    Cls = person,
    urlexp_compute(create(Cls), Path),
    member(method(post), Request),
    member(path(Path), Request),
    http_parameters(Request, [], [form_data(FormData)]),
    maplist(formdata_objprop, FormData, Object),
    database_insert(Cls, Object),
    atom_string(Cls, SCls),
    pageexp_interpret_0(Request, (
        title := "Create " + Cls
        , html(p(SCls, " created"))
    )).

dispatch(Request) :-
    member(method(get), Request),
    member(path('/'), Request),
    database_view(person, Table),
    http_parameters(Request, [
        x(X,[integer,default(0)])
        , y(Y,[integer,default(0)])
    ]),
    Z is X + Y,
    findall(Form, (class(Cls),class_html_create_form(Cls,Form)), SampleCreate),
    %class_html_create_form(person, SampleCreate),
    pageexp_interpret_0(Request, (
        title := "Title"
        , html(
            Table
            , SampleCreate
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
    )).

% -------------------- check

error(missing_class(C)) :-
    class_property(C, _),
    \+ class(C).

error(missing_property_type(C,P)) :-
    class(C),
    class_property(C, P),
    \+ class_property_type(C, P, _).

error(unregistered_locale(L)) :-
    term_locale_string(_, L, _),
    \+ globalization_locale(L).

error(missing_globalization(T,L2)) :-
    globalization_locale(L1),
    globalization_locale(L2),
    L1 \= L2,
    term_locale_string(T, L1, _),
    \+ term_locale_string(T, L2, _).

% -------------------- check

check_web_app :- \+ error(_), !.
check_web_app :-
    error(E),
    print_message(error, E),
    fail.

prolog:message(missing_property_type(C,P)) -->
    ["class_property_type/3 clause is missing for class ~w property ~w"-[C,P]].

prolog:message(unregistered_locale(L)) -->
    ["globalization_locale/1 clause is missing for locale ~w"-[L]].

prolog:message(missing_globalization(T,L)) -->
    ["term_locale_string/3 clause is missing for term ~w locale ~w"-[T,L]].
