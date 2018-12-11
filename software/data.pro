:- module(data, [
    database/2
    , table/2
    , entity_field/4
    , entity_field_nullable/3
    , sql_java_type/3
]).
/** <module> describe all data?

Intended usage:
    - enterprise web application

Usage:
    - Describe relational data.
        - Define your databases with database/2.
        - Define your tables with table/2.

Other data models:
    - ER diagram
    - SQL information schema
    - XML Schema
    - OMG QVT
    - and many others
*/

/** database(?Name, ?Spec)

"Name names a database described by Spec."

Name is an atom that is unique among all databases defined by this predicate.

Spec is a list of terms:
    - type(Type) where Type is any of these:
        - postgresql

If Spec contains type(Postgresql), then Spec may also contain these terms:
    - host(H), required: a DNS name or an IP address
    - port(P), optional: defaults to 5433
    - catalog(C), required: catalog name
        - A catalog is a collection of schemas.
        A catalog may also be called a "database", but "database" is ambiguous.
    - username(U), required
    - password(P), required

See also:
    - "What's the difference between a catalog and a schema in a relational database?"
    https://stackoverflow.com/questions/7022755/whats-the-difference-between-a-catalog-and-a-schema-in-a-relational-database
*/
:- multifile database/2.

/** table(?Name, ?Spec)

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
table(mytable, [ database(mydb),
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
:- multifile table/2.

% Records.

% match_one(Pat, List, Unmatched)

match_one(Pat, [Head | Tail], Tail) :- Pat = Head.
match_one(Pat, [Head | Tail], [Head | Unmatched]) :- match_one(Pat, Tail, Unmatched).

% match_many(Pats, List, Unmatched)

match_many([], List, Unmatched) :- List = Unmatched.
match_many([Pat | Pats], List, Unmatched) :- true
    , match_one(Pat, List, Unmatched_0)
    , match_many(Pats, Unmatched_0, Unmatched_1)
    , Unmatched_1 = Unmatched
    .

/** entity_field(?Entity, ?Name, ?Type, ?Opts).

We assume the existence of a predicate entity/2 somewhere else.

entity(Name, Fields).

- Name is an atom.
- Fields is a list.
  Each element of Fields is a list [Field_name, Field_type | Field_options].
  - Field_name is an atom.
  - Field_type:
    - The atom int32 is a Field_type.
    - If Length is a nonnegative integer, then the compound varchar(Length) is a Field_type.
    - Field_options is a list.
*/

entity_field(Entity, Name, Type, Opts) :- true
    , entity(Entity, Fields)
    , member([Name, Type | Opts], Fields)
    .

entity_field_type(Entity, Name, Type) :- entity_field(Entity, Name, Type, _).

entity_field_opts(Entity, Name, Opts) :- entity_field(Entity, Name, _, Opts).

entity_field_nullable(Entity, Name, Nullable) :- true
    , entity_field_opts(Entity, Name, Opts)
    , (member(nullable, Opts) -> Nullable = nullable ; Nullable = not_nullable).

% Mapping between SQL type and Java type.

sql_java_type_primitive(S, J) :- member((S, J), [
    (int16, short)
    , (int32, int)
    , (int64, long)
]).

% The part of the relation that is not affected by nullability.

sql_java_type_general(S, J) :- member((S, J), [
    (varchar(_), 'java.lang.String')
    , (timestamp, 'java.util.Instant')
]).

sql_java_type(S, J, not_nullable) :- false
    ; sql_java_type_primitive(S, J)
    ; sql_java_type_general(S, J)
    .

sql_java_type(S, J, nullable) :- false
    ; sql_java_type_primitive(S, P), java_primitive_reference_type(P, J)
    ; sql_java_type_general(S, J)
    .
