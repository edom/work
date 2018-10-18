:- module(data, [
    entity_field/4
    , entity_field_nullable/3
    , sql_java_type/3
]).

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

/*
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
