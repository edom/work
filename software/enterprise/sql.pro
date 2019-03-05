:- module(sql,[
    type_sqltype/2,
    recordtype_sqlcolumn/2
]).
:- use_module('./syntax.pro').
:- use_module('./type.pro').

/** type_sqltype(++TypeName,-SqlType) is semidet.

TypeName must be non-recursive.
*/
type_sqltype(T,S) :- type_normalform(T,N), type_sqltype(N,T,S).

    /** type_sqltype(++NormalForm,++TypeName,-SqlType) is semidet.

    SQL does not have unsigned types.

    In SQL, the N in varchar(N) is the maximum character count.
    Here, N is the maximum byte count.
    */
    type_sqltype(#natural,T,S) :- type_maxbitcount(T,N), 32 < N, N =< 63, !, S = bigint.
    type_sqltype(#natural,T,S) :- type_maxbitcount(T,N), 16 < N, N =< 31, !, S = integer.
    type_sqltype(#natural,_,_) :- !, false.
    type_sqltype(#integer,T,S) :- type_maxbitcount(T,N), 32 < N, N =< 64, !, S = bigint.
    type_sqltype(#integer,T,S) :- type_maxbitcount(T,N), 16 < N, N =< 32, !, S = integer.
    type_sqltype(#integer,_,_) :- !, false.
    type_sqltype(#identifier,T,S) :- type_maxbitcount(T,N), 32 < N, N =< 64, !, S = bigserial.
    type_sqltype(#identifier,T,S) :- type_maxbitcount(T,N), 0 =< N, N =< 32, !, S = serial.
    type_sqltype(#identifier,_,_) :- !, false.
    type_sqltype(#string,T,S) :- !, type_maxbytecount(T,N), S = varchar(N).
    type_sqltype(#string,_,_) :- !, false.
    type_sqltype(#optional(A),_,S) :- type_sqltype(A,S).

/** recordtype_sqlcolumn(++TypeName,-SqlColumn)

*/
recordtype_sqlcolumn(T,[name-FName,type-SType,nullable-Nullable]) :-
    recordtype_fields(T,Fields),
    fields_member(Fields,FName,FType),
    type_sqltype(FType,SType),
    once(bool(type_nullable(FType),Nullable)).

    bool(G,B) :- call(G), B = true.
    bool(_,B) :- B = false.

    type_nullable(#optional(_)) :- !.
    type_nullable(T) :- type_definition(T,D), type_nullable(D).

    fields_member(Fields,Name,Type) :- member(Name:Type, Fields).
