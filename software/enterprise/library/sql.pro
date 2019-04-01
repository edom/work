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

% -------------------- database

:- use_module(library(odbc),[
    odbc_driver_connect/3
]).

:- multifile opv/3.

:- op(100,fx,#).

get(O,P,V) :- \+ opv(O,P,V), !, existence_error(opv, opv(O,P,V)).
get(O,P,V) :- opv(O,P,V).

eval(A ? B, Z) :- !, get(A,B,Z).
eval(A + B, Z) :- !, eval(A,A0), eval(B,B0), string_concat(A0,B0,Z).
eval(A, Z) :- string(A), !, A = Z.
eval(A, _) :- !, type_error(exp, A).

test :-
    ensure(describes(con,Con)),
    do_something_with(Con),
    ensure_not(_).

% -------------------- connection management

:- dynamic known/1.
:- multifile force/1.
:- multifile force_not/1.

ensure(A) :- known(A), !.
ensure(A) :- deterministically(force(A)), !, assertz(known(A)).

% Problem: If force_not/1 throws an exception,
% then the known/1 fact may never be retracted.
ensure_not(A) :- known(A), !, deterministically(force_not(A)), retract(known(A)).
ensure_not(_).

force(describes(Rep,Con)) :-
    get(Rep, #class, connection),
    conrep_debugstr(Rep, Debug),
    debug(sql_connect, "sql_connect: Connecting to ~w", [Debug]),
    eval(
        "DRIVER=" + "{PostgreSQL Unicode}"
        + ";Server=" + Rep?host
        + ";Port=" + Rep?port
        + ";Database=" + Rep?catalog
        + ";UID=" + Rep?username + ""
        + ";PWD=" + Rep?password + ""
        , Dsn
    ),
    odbc_driver_connect(Dsn, Con, []).

force_not(describes(Rep,Con)) :-
    conrep_debugstr(Rep, Debug),
    debug(sql_connect, "sql_connect: Disconnecting from ~w", [Debug]),
    odbc_disconnect(Con).

:- debug(sql_connect). % DEBUG

conrep_debugstr(Rep, Debug) :-
    get(Rep, #class, connection),
    eval(Rep?username + "@" + Rep?host + ":" + Rep?port + ":/" + Rep?catalog, Debug).

close_all_connections :-
    forall(known(describes(Rep,Con)),
        ensure_not(describes(Rep,Con))).

:- dynamic con_sch_tab_row/4.

do_something_with(Con) :-
    retractall(con_sch_tab_row/4),
    Sql = 'SELECT table_catalog, table_schema, table_name, column_name, data_type FROM information_schema.columns',
    forall((functor(Row,row,5), odbc_query(Con,Sql,Row)),
        assertz(con_sch_tab_row(con, information_schema, columns, Row))
    ).

opv(con, P, V) :- member(P-V,[
    #class-connection
    , host-localhost
    , port-5432
    , catalog-test
    , username-test
    , password-test
]).
