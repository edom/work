:- import(file("internal/type.pro"),[
    type_natural_bit/2
    , type_integer_bit/2
    , type_identifier_bit/2
    , type_string_byte/2
    , type_optional/2
]).
:- import(file("sql_connection.pro"),[
    close_all_connections/0
    , ensure_opened/3
    , ensure_closed/1

]).
:- import(file("sql_dcg.pro"),[
    select//3
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

:- multifile opv/3.

:- op(100,fx,#).

get(O,P,V) :- \+ opv(O,P,V), !, existence_error(opv, opv(O,P,V)).
get(O,P,V) :- opv(O,P,V).

eval(A ? B, Z) :- !, get(A,B,Z).
eval(A + B, Z) :- !, eval(A,A0), eval(B,B0), string_concat(A0,B0,Z).
eval(A, Z) :- string(A), !, A = Z.
eval(A, _) :- !, type_error(exp, A).

test :-
    do_ensure_opened(con, Con),
    do_something_with(Con),
    ensure_closed(con).

do_ensure_opened(Rep, Con) :-
    get(Rep, #class, connection),
    eval(
        "DRIVER=" + "{PostgreSQL Unicode}"
        + ";Server=" + Rep?host
        + ";Port=" + Rep?port
        + ";Database=" + Rep?catalog
        + ";UID=" + Rep?username + ""
        + ";PWD=" + Rep?password + ""
        , Dsn
    ),
    ensure_opened(Rep, Dsn, Con).

% -------------------- abstracting SQL table as Prolog predicate

:- debug(sql_query). % DEBUG

sql_table_row(ConName, Schema, Table, Cols, Row) :-
    must_be(ground, ConName),
    must_be(ground, Schema),
    must_be(ground, Table),
    must_be(ground, Cols),
    once(do_ensure_opened(ConName, Con)),
    once(phrase(select(Schema,Table,Cols), Codes)),
    string_codes(Str, Codes),
    length(Cols, Arity),
    functor(Record, row, Arity),
    debug(sql_query, "sql_query: ~w", [Str]),
    odbc_query(Con, Str, Record),
    Record =.. [_|Row].

:- include("partial_evaluation.pro").

should_call(once(A)) :- should_call(A).
should_call(phrase(A,_)) :- ground(A).
should_call(string_codes(A,_)) :- ground(A).
should_call(string_codes(_,A)) :- ground(A).
should_call(length(A,_)) :- ground(A).
should_call(functor(_,B,C)) :- ground(B), ground(C).
should_call(must_be(A,B)) :- ground(A), ground(B).
should_call(A =.. _) :- nonvar(A).

should_expand(sql_table_row(_,_,_,_,_)).

% SQL-table-backed predicates are derived by partially evaluating sql_table_row/5.

sqltable_as_prologpred(Con, Sch, Tab, Cols, PredName, Clause) :-
    must_be(atom, Con),
    must_be(atom, Sch),
    must_be(atom, Tab),
    must_be(list, Cols),
    must_be(atom, PredName),
    H = sql_table_row(Con,Sch,Tab,Cols,Row),
    partial_evaluation(H, B0),
    H0 =.. [PredName|Row],
    Clause = (H0 :- B0).

make_sql_table_backed_predicate(Con, Sch, Tab, Cols, PredName) :-
    sqltable_as_prologpred(Con, Sch, Tab, Cols, PredName, Clause),
    compile_aux_clauses([Clause]).

test_part_eval :-
    Cols = [table_catalog, table_name, column_name, data_type],
    sqltable_as_prologpred(con, information_schema, columns, Cols, infosch_columns, Clause),
    portray_clause(Clause).

:-  make_sql_table_backed_predicate(con, information_schema, columns
    , [table_catalog, table_name, column_name, data_type]
    , infosch_columns).

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
