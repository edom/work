:- module(enterprise_example, [
    test/0
]).
:- use_module(library(solution_sequences)).
:- use_module('./enterprise.pro', except([name_database/2])).
:- use_module('./map.pro').
/** <module> example for enterprise.pro

See source code.

```
as postgres:
sudo su postgres
psql

CREATE USER my_user PASSWORD 'my_password';
CREATE DATABASE my_database OWNER my_user;

as my_user:
psql -h localhost my_database my_user

CREATE SCHEMA my_schema;
```
*/

enterprise:name_database(N,D) :- name_database(N,D).

name_database(db_mydb, [type(postgresql), host('localhost'),
    catalog(my_database), username(my_user), password('my_password')
]).

test :-
    with_name_connect(db_mydb, Con, (
        odbc_columns(Con, Columns),
        columns_tables(Columns, Tables),
        map(Tab, (print(Tab), nl, nl), Tables)
    )).

odbc_columns(Con, Columns) :-
    Sql = 'SELECT table_catalog, table_schema, table_name, column_name, ordinal_position, data_type FROM information_schema.columns',
    findall([catalog(Catalog), schema(Schema), table(Table), column(Column), position(Pos), type(Type)],
        odbc_query(Con, Sql, row(Catalog, Schema, Table, Column, Pos, Type)),
        Columns
    ).

% WIP
columns_tables(Cols, Tabs) :-
    map(Col, (Sch-Tab)-Col, ( member(schema(Sch),Col), member(table(Tab),Col) ), Cols, Tabs0),
    sort(Tabs0, Tabs1),
    group_pairs_by_key(Tabs1, Tabs).

% WIP
infoschemacolumn_prologclause(Column, Clause) :-
    member(catalog(Catalog), Column),
    member(schema(Schema), Column),
    member(table(Table), Column),
    Clause = column(Catalog, Schema, Table).
