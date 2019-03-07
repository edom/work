type_javatype(T,J) :- type_integer_bit(T,N), 0 =< N, N =< 32, !, J = int.
type_javatype(T,J) :- type_integer_bit(T,N), 32 < N, N =< 64, !, J = long.
type_javatype(T,J) :- type_identifier_bit(T,N), 0 =< N, N =< 32, !, J = int.
type_javatype(T,J) :- type_identifier_bit(T,N), 32 < N, N =< 64, !, J = long.
type_javatype(T,J) :- type_string(T), !, J = 'java.lang.String'.
type_javatype(T,J) :- type_optional(T,A), !, type_javatype(A,P), javatype_javareftype(P,J).
type_javatype(T,_) :- throw(error(no_related_java_type_for_type(T),_)).

    javatype_javareftype(boolean,R) :- !, R = 'java.lang.Boolean'.
    javatype_javareftype(char,R) :- !, R = 'java.lang.Character'.
    javatype_javareftype(void,R) :- !, R = 'java.lang.Void'.
    javatype_javareftype(byte,R) :- !, R = 'java.lang.Integer'.
    javatype_javareftype(short,R) :- !, R = 'java.lang.Short'.
    javatype_javareftype(int,R) :- !, R = 'java.lang.Integer'.
    javatype_javareftype(long,R) :- !, R = 'java.lang.Long'.
    javatype_javareftype(float,R) :- !, R = 'java.lang.Float'.
    javatype_javareftype(double,R) :- !, R = 'java.lang.Double'.
    javatype_javareftype(A,A).

typename_javaclassname(T,J) :-
    atom_codes(T,C),
    capitalize(C,Cap),
    atom_codes(J,Cap).

    capitalize([H|T],[H0|T]) :- code_type(H0,to_upper(H)).
    capitalize([],[]).
