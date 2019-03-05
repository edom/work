:- module(link, [
    load_spec/1,
    load_specs/1
]).
:- use_module('./syntax.pro').
/** <module> Linking
*/

/** load_spec(++Path)

Path is an atom that is a relative file path.
*/
load_spec(Path) :-
    Module = Path,
    add_import_module(Module,syntax,start),
    Module:consult(Path),
    link(type,Module,[
        type_definition/2
        , type_maxbitcount/2
        , type_maxbytecount/2
    ]),
    true.

    link(Owner,Guest,[]) :- !.
    link(Owner,Guest,[H|T]) :- !, link(Owner,Guest,H), link(Owner,Guest,T).
    link(Owner,Guest,Name/Arity) :- !,
        print_message(informational,link(Owner,Guest,Name/Arity)),
        functor(Functor,Name,Arity),
        Owner:assertz(Functor :- Guest:Functor).

load_specs([]) :- !.
load_specs([H|T]) :- !, load_spec(H), load_specs(T).

prolog:message(link(Owner,Guest,Pred)) -->
    ['Linking: ~p:~p :- ~p:~p'-[Owner,Pred,Guest,Pred]].
