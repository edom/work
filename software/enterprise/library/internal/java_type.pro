% ------- map from types to Java types

type_javatype(T,J) :- type_integer_bit(T,N), 0 =< N, N =< 32, !, J = int.
type_javatype(T,J) :- type_integer_bit(T,N), 32 < N, N =< 64, !, J = long.
type_javatype(T,J) :- type_identifier_bit(T,N), 0 =< N, N =< 32, !, J = int.
type_javatype(T,J) :- type_identifier_bit(T,N), 32 < N, N =< 64, !, J = long.
type_javatype(T,J) :- type_string(T), !, J = 'java.lang.String'.
type_javatype(T,J) :- type_optional(T,A), !, type_javatype(A,P), javatype_javareftype(P,J).
type_javatype(T,_) :- throw(error(no_related_java_type_for_type(T),_)).

class_qualified_name(C,Q) :-
    class_package_name(C,P),
    class_name(C,N),
    atomic_list_concat([P,'.',N],Q).

java_class_type(C,T) :- class_qualified_name(C,T).
