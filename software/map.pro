:- module(map, [
    map/5
    , map/4
    , map/3
]).

/*
Make assert work.
*/

:- meta_predicate
    map(?, 0, ?)
    , map(?, ?, ?, ?)
    , map(?, ?, 0, ?, ?)
    .

/*
map(In, Out, Goal, Ins, Outs).

In should be a variable that occurs in Goal.

Out should be a variable that occurs in Goal.

In is repeatedly unified with the elements of Ins.

Example:
?- Xs = [1,2,3], map(X, Y, Y #= X + 1, Xs, Ys).
That produces Ys = [2,3,4].

Example:
f(X, Y) :- Y #= X + 1.
f(X, Y) :- Y #= X + 2.
?- Xs = [1,2,3], map(X, Y, f(X,Y), Xs, Ys).
That produces Ys = [2,3,3,4,4,5].

Grounding the In variable filters the output Outs.
Example:
f(X, Y) :- Y #= X + 1.
f(X, Y) :- Y #= X + 2.
?- Xs = [1,2,3], X #=< 2, map(X, Y, f(X,Y), Xs, Ys).
That produces Ys = [2,3,3,4].
*/
map(In, Out, Goal, Ins, Outs) :- findall(Out, (member(In, Ins), Goal), Outs).

map(In, Out, Ins, Outs) :- findall(Out, member(In, Ins), Outs).

map(In, Goal, Ins) :- forall(member(In, Ins), Goal).
