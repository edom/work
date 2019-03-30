:- module(odbc_postgresql, [
    main/0
    , ast_sql/2
]).
/** <module> work with PostgreSQL via ODBC
*/

:- use_module('./map.pro').

/*
For PostgreSQL configuration options, see:
https://odbc.postgresql.org/docs/config-opt.html
*/
main :-
    %odbc_driver_connect('DRIVER={PostgreSQL Unicode};Server=localhost;Port=5432;Database=<DATABASE>;UID=<USER>;PWD=<PASSWORD>;', Con, []),
    odbc_driver_connect('DRIVER={PostgreSQL Unicode};Server=localhost;Port=5432;Database={my_database};UID={my_user};PWD={my_password};', Con, []),
    Sql = 'SELECT table_catalog, table_schema, table_name, column_name, data_type FROM information_schema.columns',
    findall(Row, (compound_name_arity(Row, row, 5), odbc_query(Con, Sql, Row)), Rows),
    print(Rows),
    odbc_disconnect(Con).


ast_sql(select(Cols, TableExp), Sql) :-
    ast_sql(select(Cols, TableExp, true), Sql).

ast_sql(select(Cols, TableExp, WhereExp), Sql) :-
    ast_Cols__sql_(Cols, SqlCols),
    ast_TableExp__sql_(TableExp, SqlTableExp),
    ast_Exp__sql_(WhereExp, SqlWhereExp),
    atomics_to_string(["SELECT ", SqlCols, " FROM ", SqlTableExp, " WHERE ", SqlWhereExp], Sql).


ast_Cols__sql_(Cols, Sql) :-
    atomics_to_string(Cols, ', ', Sql).


% TODO Escape.
ast_TableExp__sql_(Name, Name) :- atom(Name), !.

ast_TableExp__sql_(as(Exp,Alias), Sql) :- !,
    ast_TableExp__sql_(Exp, SE),
    atomics_to_string([SE, ' AS ', Alias], Sql).

ast_TableExp__sql_(join(Ts), Sql) :- !,
    map(T, ST, ast_TableExp__sql_(T, ST), Ts, STs),
    atomics_to_string(STs, ', ', Sql).


ast_Exp__sql_(true, "TRUE") :- !.
ast_Exp__sql_(Col, S) :- atom(Col), !, atom_string(Col, S).
ast_Exp__sql_(Int, S) :- integer(Int), !, number_string(Int, S).
ast_Exp__sql_(Exp2, S) :-
    compound_name_arity(Exp2, Op, 2), !,
    arg(1, Exp2, A),
    arg(2, Exp2, B),
    ast_Exp__sql_(A, SA),
    ast_Exp__sql_(B, SB),
    operator_sql_(Op, SOp),
    atomics_to_string(['(', SA, ' ', SOp, ' ', SB, ')'], S).

operator_sql_(A, B) :- operator_sql_different_(A, B), !.
operator_sql_(A, A).

operator_sql_different_(/\, 'AND').
operator_sql_different_(\/, 'OR').
operator_sql_different_(\+, 'NOT').
