my_check :-
    forall(error(E), print_message(error,E)).

error(importing_unexported_predicate(I,E,P)) :-
    object_import(I,E,P),
    \+ object_export(E,P).
