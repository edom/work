:- module(database_internal_pure, [
    rethrow/1,
    catch3/3,
    catch_all/2,
    always/1
]).
/** <module> database.pro internals; do not use
*/

rethrow(Es) :- throw(error(many(Es),_)).

catch3(Goal, E -> B, Ok) :-
    catch(Goal, E, true),
    (var(E) -> Ok ; B).

catch_all([], []).
catch_all([G|Gs], Es) :- catch3(always(G), E -> (Es = [E|E1], catch_all(Gs,E1)), catch_all(Gs,Es)).

always(G) :- G, ! ; true.
