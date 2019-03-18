state_object(S, named(state,S)) :- state(S).

object(T) :- state_object(_,T).

object_property(T, Prop) :-
    state_object(_,T),
    member(Prop, [
        name
        , type
        , initializer
    ]).

opv(T, name, Val) :- state_object(S, T), state_name(S, Val).
opv(T, type, Val) :- state_object(S, T), state_type(S, Val).
opv(T, initializer, Val) :- state_object(S, T), state_initializer(S, Val).

