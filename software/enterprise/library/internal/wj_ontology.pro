state_object(S, named(state,S)) :- state(S).

object(T) :- state_object(_,T).

object_property(T, Prop) :-
    state_object(_,T),
    member(Prop, [
        name
        , type
        , initializer
    ]).

object_property_value(T, name, Val) :- state_object(S, T), state_name(S, Val).
object_property_value(T, type, Val) :- state_object(S, T), state_type(S, Val).
object_property_value(T, initializer, Val) :- state_object(S, T), state_initializer(S, Val).

