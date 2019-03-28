interpret(A, Z) :- interpret([], A, Z).

% empty object
interpret(_, empty, Z) :- !, Z = [].

% singleton object
interpret(_, singleton(K,V), Z) :- !, Z = [K-V].

% object literal
interpret(_, object(A), Z) :- !,
    (is_object(A) -> true ; type_error(object,A)),
    Z = A.

% object union
interpret(C, A+B, Z) :- !, interpret(C,A,A0), interpret(C,B,B0), append(A0,B0,Z).

% object mutation
interpret(C, ObjExp/(Key-Value), List) :- !,
    interpret(C, ObjExp, Object),
    upsert(Object, Key, Value, List).

interpret(C, ObjExp/(M1,M2), Z) :- !,
    interpret(C, (ObjExp/M1)/M2, Z).

% object property
interpret(C, ObjExp:Key, Value) :- !,
    interpret(C, ObjExp, Object),
    (\+ member(Key-_, Object) -> existence_error(property,Key) ; true),
    member(Key-Value, Object).

interpret(_, A, _) :- !, type_error(expression, A).

is_object([]).
is_object([_-_|A]) :- is_object(A).

upsert([], K, V, Z) :- !, Z = [K-V].
upsert([K-_|O], K, V, Z) :- !, Z = [K-V|O].
upsert([A|O], K, V, Z) :- !, Z = [A|B], upsert(O, K, V, B).
upsert(A, _, _, _) :- !, type_error(list, A).