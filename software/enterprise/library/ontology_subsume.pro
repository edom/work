% -------------------- spo/3 subsumes opv/3

spo(S, has(P), O) :- opv(S, P, O).

% -------------------- opv/3 subsumes object_proplist/2

:- include("proplist.pro").

opv(Obj, Prop, Val) :-
    object_proplist(Obj, List),
    proplist_key_value(List, Prop, Val).
