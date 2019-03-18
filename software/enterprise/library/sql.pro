% Translate type to SQL type.

/** type_sqltype(++TypeName,-SqlType) is det.

TypeName must be non-recursive.

SQL does not have unsigned types.

In SQL, the N in varchar(N) is the maximum character count.
Here, N is the maximum byte count.
*/

type_sqltype(T, S) :- type_natural_bit(T, N), 32 < N, N =< 63, !, S = bigint.
type_sqltype(T, S) :- type_natural_bit(T, N), 16 < N, N =< 31, !, S = integer.
type_sqltype(T, S) :- type_integer_bit(T, N), 32 < N, N =< 64, !, S = bigint.
type_sqltype(T, S) :- type_integer_bit(T, N), 16 < N, N =< 32, !, S = integer.
type_sqltype(T, S) :- type_identifier_bit(T, N), 32 < N, N =< 64, !, S = bigserial.
type_sqltype(T, S) :- type_identifier_bit(T, N), 0 =< N, N =< 32, !, S = serial.
type_sqltype(T, S) :- type_string_byte(T, N), !, S = varchar(N).
type_sqltype(T, S) :- type_optional(T, A), !, type_sqltype(A, S).
type_sqltype(T, _) :- !, throw_error(type_not_realizable(T)).

/** recordtype_sqlcolumn(++TypeName,-SqlColumn)

SQL nullability is a property of the column, not of the column type.
*/

recordtype_sqlcolumn(T,[name-FName,type-SType,nullable-Nullable]) :-
    recordtype_fields(T,Fields),
    fields_member(Fields,FName,FType),
    type_sqltype(FType,SType),
    once(bool(type_nullable(FType),Nullable)).

    bool(G,B) :- call(G), B = true.
    bool(_,B) :- B = false.

    type_nullable(T) :- type_optional(T, _).

    fields_member(Fields,Name,Type) :- member(Name:Type, Fields).
