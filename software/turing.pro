% Turing machine simulator?

cons(H,T,[H|T]).

output(init,H,H).
output(halt,H,H).
output(w0,_H,0).
output(w1,_H,1).

% transition(State,Head,NewState,NewHead,Dir)
transition(init, H, w0, H, right).
transition(w0, _, w1, 0, right).
transition(w1, _, halt, 1, right).
transition(halt, H, halt, H, stay).

stay(halt,_H).

left(_S,_H) :- fail.

right(init,_H).
right(w0,_H).
right(w1,_H).

jump(init,_H,w0).
jump(w0,_H,w1).
jump(w1,_H,halt).
jump(halt,_H,halt).

tape(S,(L,H,R),(L,W,R)) :- output(S,H,W), stay(S,H).
tape(S,(L,H,R),(LL,HH,RR)) :- output(S,H,W), left(S,H), cons(HH,LL,L), cons(W,R,RR).
tape(S,(L,H,R),(LL,HH,RR)) :- output(S,H,W), right(S,H), cons(W,L,LL), cons(HH,RR,R).

phase((S,(L,H,R)),(SS,(LL,HH,RR))) :- jump(S,H,SS), tape(S,(L,H,R),(LL,HH,RR)).

% phase((S,(L,H,R)),(SS,(LL,HH,RR))) :- transition(S,H,SS,HH,Dir).

run(P,Ps) :- phase(P,Q), P \= Q, !, run(Q,Qs), Ps = [P|Qs].
run(P,[P]).

% B = [b|B], run((init,(B,0,B)),Path).
