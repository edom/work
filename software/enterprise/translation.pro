/*
We need to generate the auxiliary predicate input/2
because reading may be recursive and Prolog does not have anonymous recursion.
*/
makeinputcli(#identifier, (input(#identifier,'$VAR'('Value')) :- !, read('$VAR'('Value')), ground('$VAR'('Value')))).
makeinputcli(#natural, (input(#natural,'$VAR'('Value')) :- !, read('$VAR'('Value')), integer('$VAR'('Value')), '$VAR'('Value') >= 0)).
makeinputcli(#string, (input(#string,'$VAR'('Value')) :- !, read('$VAR'('Value')), string('$VAR'('Value')))).
makeinputcli(RecType, (input(RecType,'$VAR'('Value')) :- !, ReadFields, '$VAR'('Value') =.. [entry|Args])) :-
    recordtype_fields(RecType,Fields),
    makeinput0(Fields,ReadFields,Args).
makeinputcli(Type, (input(Type,'$VAR'('Value')) :- !, input(Def,'$VAR'('Value')))) :-
    type_definition(Type,Def), \+ Def = #record(_).

    recordtype_fields(Type,Fields) :- type_definition(Type,#record(Fields)).

    % Need a more user-friendly prompt.
    makeinput0([Name:Type], (write(Name), nl, input(Type,A)), [A]) :- !.
    makeinput0([Name:Type|Fields], (write(Name), nl, input(Type,A), Prog), [A|Args]) :- !, makeinput0(Fields,Prog,Args).
    makeinput0([],true,[]) :- !.

usecase_program(U, usecase_interpret(U) :- P) :-
    usecase_definition(U,D),
    ucdef_prolog(D,P).

    ucdef_prolog(input(T,V), P) :- !, P = input(T,V).
    ucdef_prolog(insert1(S, Needle), P) :- !, P = assertz(database_entry(S,Needle)).
    ucdef_prolog((A,B), (A0,B0)) :- !, ucdef_prolog(A,A0), ucdef_prolog(B,B0).
    ucdef_prolog(U,_) :- !, error(invalid_term_in_use_case_definition(U)).

error(E) :- throw(error(E,_)).

generate :-
    genprog([
        (input('$VAR'('Type'),_) :- \+ground('$VAR'('Type')), !, throw(error(input_type_not_ground('$VAR'('Type')),_))),
        #all(P,makeinputcli(_,P)),
        (input('$VAR'('Type'),_) :- !, throw(error(input_unknown_type('$VAR'('Type')),_)))
    ]).

    genprog([]) :- !.
    genprog([H|T]) :- !, genprog(H), genprog(T).
    genprog(#all(A,G)) :- call(G), genwrite(A), fail.
    genprog(#all(_,_)) :- !.
    genprog(A) :- genwrite(A).

    % Is there no pretty-printer for Prolog source?
    genwrite(A) :- numbervars(A), write_term(A, [numbervars(true), quoted(true), fullstop(true), nl(true)]).
