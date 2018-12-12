:- module(enterprise, [
    name_database/2
    , name_table/2
    , name_connect/2
    , with_name_connect/3
    , connection_close/1
]).
:- use_module(library(odbc)).
/** <module> Prolog for enterprise applications?
Usage:
    - Describe relational data stored in, most likely, an SQL database.
        - Define your databases with name_database/2.
        - Define your tables with name_table/2.
    - Low-level
        - Connect to database with with_name_connect/3.
        If you are using Debian, make sure that the odbc-postgresql package is installed.
    - Ideas?
        - Connection-pooling meta-interpreter? dsn_connection(++Dsn, --Con)
        - Query language that can describe cross-database application-level join.

See enterprise_example.pro for an example.

Other similar data models:
    - ER diagram
    - SQL information schema
    - XML Schema
    - OMG QVT
    - and many others
*/

/** name_database(?Name, ?Spec)

"Name names a database described by Spec."

Name is an atom that should be unique among all databases defined by this predicate.

Spec is a list of terms:
    - type(Type) where Type is any of these:
        - postgresql: PostgreSQL 9.5 or newer (for update-or-insert)

If Spec contains type(postgresql), then Spec must/may also contain these terms:
    - host(H), required: H is an atom representing a DNS name such as 'example.com' or an IP address such as '1.2.3.4'
    - port(P), optional: P is an integer that defaults to 5432
    - catalog(C), required: catalog name: C is an atom
        - A catalog is a collection of schemas.
        A catalog may also be called a "database", but "catalog" is less ambiguous.
    - username(U), atom, required
    - password(P), atom, required

Example:
```
enterprise:name_database(N,D) :- name_database(N,D).
name_database(db_mydb, [type(postgresql), host('localhost'), port(5432),
    catalog(my_catalog), username('my_user'), password('my_password')
]).
```

See also:
    - What's the difference between a catalog and a schema in a relational database?
    https://stackoverflow.com/questions/7022755/whats-the-difference-between-a-catalog-and-a-schema-in-a-relational-database
*/
:- multifile name_database/2.

/** name_table(?Name, ?Spec)

Name is an atom that is unique among all tables defined by this predicate.

Spec is a list of terms:
    - database(D), where D is the name of a database defined using database/2
    - column(ColName, ColSpec), may occur zero or more times
        - ColName has to be unique among columns in the same table
        - ColSpec is a list of these terms:
            - name_in_database(DbColName), optional: column name in the database.
            This defaults to ColName.
            - type(Type), required: Type is any of these:
                - =int16=: two's-complement signed 16-bit integer
                - =int32=: two's-complement signed 32-bit integer
                - =int64=: two's-complement signed 64-bit integer
                - =float32=: IEEE 754 single-precision floating-point integer (1 sign bit, 8 exponent bits, 23 fraction bits)
                - =float64=: IEEE 754 double-precision floating-point integer (1 sign bit, 10 exponent bits, 53 fraction bits)
                - =chars=: a character string
                - =bytes=: a byte string
            - max_bytes(N), optional: indicates that each cell contains at most N bytes.
            This is for the =chars= and =bytes= type.
                - Note: bytes, not characters.
                The number of bytes in a character string depends on its character encoding.
            - nullable(N), optional: N is either true or false.
            Unlike in SQL, this defaults to _false_.

```
name_table(mytable, [ database(mydb),
    column(mycolumn, [type(int32)])
]).
```

See also:
    - https://wiki.postgresql.org/wiki/BinaryFilesInDB

Design:
What do I think about this?
```
sql_server(prod, [type(postgresql), host('localhost'), port(5433)])
sql_connection(prod, [server(prod), login('john', 'foo'), database(bar), schema(company)])
sql_table(employee, [connection(prod), column(name,varchar(30),not_null)]).

```
*/
:- multifile name_table/2.

/** name_connect(++Name, --Con)

Open a new connection Con to database described by Name.

Close Con with connection_close/1 after you have finished using it.

Security notes:
    - The driver string is not sanitized.
    The driver string parameters are assumed to come from the programmer that uses this library.

See also:
    - library(odbc)
    - odbc_driver_connect/3
    - odbc_connect/3 seems to translate DSN (Data Source Name) in an INI file to Driver String
    - odbc_disconnect/1
    - SWI-prolog odbc.c https://github.com/SWI-Prolog/packages-odbc/blob/master/odbc.c
*/
name_connect(Name, Con) :-
    ( name_database(Name, D) -> true ; throw(error(unknown_database_name(Name),_)) ),
    database_odbcconnect_(D, Con).

database_odbcconnect_(D, Con) :-
    member(type(Type), D),
    member(host(Host), D),
    member(catalog(Catalog), D),
    member(username(User), D),
    member(password(Pass), D), odbc_escape_string_(Pass, EPass),
    ( type_driver_(Type, Driver) -> true ; throw(error(unknown_database_type(Type),_)) ),
    odbc_escape_string_(Driver, EDriver),
    database_port_(D, Port),
    atomic_list_concat([
        'DRIVER=',EDriver,
        ';Server=',Host,
        ';Port=',Port,
        ';Database=',Catalog,
        ';UID=',User,
        ';PWD=',EPass,
        ';'
    ], DriverString),
    odbc_driver_connect(DriverString, Con, []).

odbc_escape_code_(0'}, [0'}, 0'}]) :- !.
odbc_escape_code_(A, [A]).

odbc_escape_codes_([H|T], E) :- odbc_escape_code_(H,EH), append(EH,ET,E), !, odbc_escape_codes_(T,ET).
odbc_escape_codes_([], []).

/** odbc_escape_string_(++Unescaped, --Escaped)

Unescaped is atom or string.

Escaped is string.

ODBC escaping problems:
    - https://stackoverflow.com/questions/22398212/escape-semicolon-in-odbc-connection-string-in-app-config-file/22398538
    - https://github.com/mkleehammer/pyodbc/issues/232
*/
odbc_escape_string_(In, ES) :-
    (atom(In) -> atom_string(In, S) ; In = S),
    string_codes(S, C),
    odbc_escape_codes_(C, EC),
    string_codes(ES0, EC),
    string_concat("{", ES0, ES1),
    string_concat(ES1, "}", ES).

database_port_(D, P) :- member(port(P), D), !.
database_port_(D, 5432) :- member(type(postgresql), D), !.

type_driver_(postgresql, 'PostgreSQL Unicode').

:- multifile prolog:message/3.
prolog:message(error(unknown_database_name(Name), _)) --> [ 'Database name not defined in name_database/2: ~w'-[Name] ].
prolog:message(error(unknown_database_type(Type), _)) --> [ 'Unknown database type: ~w'-[Type] ].

/** connection_close(++Con)

Close the connection.
*/
connection_close(Con) :- odbc_disconnect(Con).

/** with_name_connect(++Name, --Con, ++Goal)

See also name_connect/2 and setup_call_cleanup/3.
*/
:- meta_predicate with_name_connect(?, ?, 0).
with_name_connect(Name, Con, Goal) :-
    setup_call_cleanup(name_connect(Name, Con), Goal, odbc_disconnect(Con)).
