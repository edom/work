:- import(file("internal/type.pro"),[
    type_natural_bit/2
    , type_integer_bit/2
    , type_identifier_bit/2
    , type_string_byte/2
    , type_optional/2
]).
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

% -------------------- Data Definition Language

:- multifile
    class/1,
    class_property/2,
    class_property_type/3.

:- import(file("../../string0.pro"),[
    strings_separator_join/3
    , strings_join/2
]).

sql_ddl_create_table(Cls, Sql) :-
    class(Cls),
    findall(Col, sql_ddl_column(Cls, _, Col), Cols),
    strings_separator_join(Cols, ", ", CommaSepCols),
    strings_join(["CREATE TABLE ", Cls, " (", CommaSepCols, "\n);"], Sql).

sql_ddl_column(Cls, Prop, Sql) :-
    class(Cls),
    class_property(Cls, Prop),
    class_property_type(Cls, Prop, Type),
    type_sqltype(Type, SqlType),
    format(string(Sql), "~n~4|~w ~w NOT NULL", [Prop,SqlType]).
