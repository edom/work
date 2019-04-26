:- annotate(file,[
    purpose-"help people maintain big Prolog programs"
]).

:- section("browse, discover, find, inquire, navigate, search, query").

object_purpose(Obj, Str) :-
    object_annotation(Obj, Ann),
    member(purpose-Str, Ann).

object_problem(Obj, Str) :-
    object_annotation(Obj, Ann),
    member(problem-Str, Ann).

obj_ann_1(O,A) :-
    object_annotation(O,As),
    member(A,As).

object_tag(Object, Tag) :-
    obj_ann_1(Object, tags-Tags),
    member(Tag, Tags).

/*
object(Object) :-
    setof(O, A^object_annotation(O,A), Os),
    member(Object, Os).
*/

object(file(F)) :- file_visited(F).
object(file_predicate(F,P)) :- file_predicate(F,P).

:- annotate([problem-"this is like drinking from the firehose"]).

browse :-

    format("~`-t~40| problems~n",[]),
    forall(object(Object), (
        object_problem(Object, _)
        ->  format("~p~n", [Object]),
            forall(object_problem(Object, Problem),
                format("~4|- ~w~n", [Problem])
            )
        ;   true
    )),

    format("~`-t~40| files~n",[]),
    forall((object(Object), Object = file(_)), (
        (object_purpose(Object, Purpose) -> true ; Purpose = ''),
        format("~p~88|~w~n", [Object,Purpose])
    )),

    format("~`-t~40| predicates~n",[]),
    forall((object(Object), Object = file_predicate(_,_)), (
        (object_purpose(Object, Purpose) -> true ; Purpose = ''),
        format("~p~88|~w~n", [Object,Purpose])
    )),

    format("~`-t~40| tagged objects~n",[]),
    forall(tag(Tag), (
        (tag_description(Tag,Description)
        ->  format("~w (~w)~n", [Tag,Description])
        ;   format("~w~n", [Tag])
        ),
        (object_tag(Object, Tag)
        ->  format("~4|- ~p~n", [Object])
        ;   true
        )
    )).

    tag(Tag) :-
        setof(T, O^object_tag(O,T), Tags),
        member(Tag, Tags).


:- end_section.


:- section("tags").

tag_description(java,"Java programming language").
tag_description(jvm,"Java Virtual Machine").

:- end_section.


:- section("experiment").

my_call(E:G) :-
    eval_mod_exp(E,M),
    call(M:G).

eval_mod_exp(file(Rel), Mod) :- !,
    must_be(ground, Rel),
    absolute_file_name(Rel, Abs),
    (unit_module(Abs, Mod)
    ->  true
    ;   throw_error(file_not_loaded(Rel))).

eval_mod_exp(Exp, _) :-
    type_error(module_expression, Exp).

:- end_section.


:- section("test").

dump :-
    repeat, (
        unit_term(F,I,T,_),
        format("~`=t~30| ~w#~w~n",[F,I]),

        format("~`-t~30| unit_term/4~n",[]),
        portray_clause(T),

        term_clause(T,C),
        format("~`-t~30| term_clause/2~n",[]),
        portray_clause(C),

        link_unit_clause(F,C,L),
        format("~`-t~30| link_unit_clause/3~n",[]),
        portray_clause(L),

        fail
    ;   !
    ).

:- end_section.
