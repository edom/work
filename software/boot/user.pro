:- annotate(file,[
    purpose-"help people maintain big Prolog programs"
]).

:- import(user,[
    file_visited/1
    , file_predicate/2
    , object_annotation/2
    , throw_error/1
]).

:- export([
    interact/0
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

object(file(F)) :- file_visited(F).
object(file_predicate(F,P)) :- file_predicate(F,P).

:- end_section.


:- section("text user interface").

%%  command(?Context, ?Command, ?Goal)
command(h, show_help).
command(q, quit).
command(sf, show_files).
command(sp, show_predicates).
command(sq, show_problems).
command(sto, show_tagged_objects).

show_help :-
    forall(command(Term,Goal), (
        % Requiring "~0|" after in format/2 after read/1 seems to be a programming error.
        format("~0|~w~t~16|~w~n", [Term,Goal])
    )).

handle_command(Term) :-
    command(Term, Goal)
    ->  call(Goal)
    ;   format("unknown command: ~w~n", [Term]).

show_files :-
    format("~0|~`-t~40| files~n",[]),
    forall((object(Object), Object = file(File)), (
        (object_purpose(Object, Purpose) -> true ; Purpose = ''),
        format("~w~88|~w~n", [File,Purpose])
    )).

show_predicates :-
    format("~0|~`-t~40| predicates~n",[]),
    forall((object(Object), Object = file_predicate(_,_)), (
        (object_purpose(Object, Purpose) -> true ; Purpose = ''),
        format("~p~88|~w~n", [Object,Purpose])
    )).

show_problems :-
    format("~0|~`-t~40| problems~n",[]),
    forall(object(Object), (
        object_problem(Object, _)
        ->  format("~p~n", [Object]),
            forall(object_problem(Object, Problem),
                format("~4|- ~w~n", [Problem])
            )
        ;   true
    )).

show_tagged_objects :-
    format("~0|~`-t~40| tagged objects~n",[]),
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

quit :- fail.

interact :-
    writeln("---------------------------------------- interactive prompt"),
    writeln(""),
    writeln("Enter 'h.' without quotes for help"),
    writeln(""),
    '$interact'.

'$interact' :-
    read(Command),
    (Command == end_of_file
    ->  true
    ;   (handle_command(Command)
        ->  '$interact'
        ;   true
        )
    ).

:- end_section.


:- section("tags").

tag_description(java,"Java programming language").
tag_description(jvm,"Java Virtual Machine").

:- end_section.
