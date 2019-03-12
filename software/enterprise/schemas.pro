:- module(schemas,[
    generate/0
]).

/** <module> Schema definitions (sketch)

*/



% -------------------- type



java_access_modifier(public).
java_access_modifier(protected).
java_access_modifier(package).
java_access_modifier(private).

stringy(A) :- atom(A).
stringy(A) :- string(A).

any(_).

boolean(false).
boolean(true).

or([],A) :- !, false.
or([T|Ts],A) :- call(T,A), !.
or([_|Ts],A) :- or(Ts,A).



% -------------------- schema



schema_shorthand(java_program,[
    maven_coordinates(group_id:stringy, artifact_id:stringy, version:stringy) - [[+,+,+]-nondet]
    , element(elem_id:term) - [[?]-nondet, [+]-semidet]
    , element_access(elem_id:term, access:java_access_modifier) - [[+,-]-semidet]
    , element_final(elem_id:term, final:boolean) - [[+,-]-semidet]
]).

predicate_derived(S-shorthand-Name/Arity, Name, Arity, Params, Modes) :-
    schema_shorthand(S, Preds),
    member(Head - Modes, Preds),
    functor(Head, Name, Arity),
    Head =.. [_ | Params].

predicate_name_arity(P, Name, Arity) :-
    predicate_derived(P, Name, Arity, _, _).

predicate_parameter(P, Order, Name, Type) :-
    predicate_derived(P, _, _, Params, _),
    nth1(Order, Params, Param),
    (Param = Name:Type -> true ; Name = Param, Type = any).

predicate_line(Id, Name, Params, Mode, Det) :-
    predicate_derived(Id, Name, _, Params, Modes),
    member(Mode-Det, Modes).


/** generate is det.

Generate.
*/
generate :-
    predicate_line(_,Name,Params,Dirs,Det),
    format("~w(", [Name]),
    write_params(Dirs,Params),
    format(") is ~w~n", [Det]),
    fail.

    write_params([],[]) :- !.
    write_params([Dir],[Param]) :- !,
        write_param(Dir,Param).
    write_params([Dir|Dirs],[Param|Params]) :- !,
        write_param(Dir,Param), write(", "),
        write_params(Dirs,Params).
    write_params(A,B) :- !,
        throw(error(lists_lengths_differ(A,B),_)).

    write_param(Dir,Name:Type) :-
        capitalize_atom(Name,UName),
        format("~w~w:~w",[Dir,UName,Type]).

    capitalize_atom(A,B) :- atom_codes(A,C), capitalize_codes(C,D), atom_codes(B,D).

        capitalize_codes([],[]) :- !.
        capitalize_codes([A|Z],[B|Z]) :- !, code_type(B,to_upper(A)).


/** schema(?SchemaId) is nondet.
    schema_predicate(?SchemaId,?PredId) is nondet.
    schema_optional(?SchemaId,?PredId) is nondet.
    schema_multifile(?SchemaId,?PredId) is nondet.

*/

/** predicate(?PredId) is nondet.
    predicate_name_arity(?PredId,?Name,?Arity) is nondet.
    predicate_parameter(?PredId,?Order,?Name,?Type) is nondet.
    predicate_mode(?PredId,?ParamDirs,?Determinism) is nondet.

Determinism is `det`, `semidet`, `nondet`, or `failure`.
*/



/*
Pldoc "dynamic documentation" hack.
See also the internals in pldoc/doc_process.pl.
*/

:- multifile '$pldoc'/4.

'$pldoc'(
    foo/1,
    '/home/erik/work/software/enterprise/schemas.pro':1,
    "Summary of foo/1",
    "%! foo(?What) is nondet.
% This should show in private documentation.
"
).

'$pldoc'(
    foo/2,
    '/home/erik/work/software/enterprise/schemas.pro':1,
    "Summary of foo/2",
    "%! foo(?Who,?What) is nondet.
% This should also show in private documentation.
"
).
