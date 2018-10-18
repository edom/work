:- module(match, [
    match_one/3
    , match_many/3
]).

% Records.

% match_one(Pat, List, Unmatched)

match_one(Pat, [Head | Tail], Tail) :- Pat = Head.
match_one(Pat, [Head | Tail], [Head | Unmatched]) :- match_one(Pat, Tail, Unmatched).

% match_many(Pats, List, Unmatched)

match_many([], List, Unmatched) :- List = Unmatched.
match_many([Pat | Pats], List, Unmatched) :- true
    , match_one(Pat, List, Unmatched_0)
    , match_many(Pats, Unmatched_0, Unmatched_1)
    , Unmatched_1 = Unmatched
    .
