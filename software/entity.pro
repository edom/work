/** <module> somewhat limited metamodel

*/
:- module(entity, [
    entity/2
    , entity_property_type/3
]).

/** entity(?Name:atom, ?Properties:list)

"Name refers to an entity whose properties are Properties."

Name must be globally unique in a Prolog process.

Properties is a list where
    - Each element has shape property(PropName, PropList) where
        - PropName has to be unique in one entity, but doesn't have to be globally unique
        - PropList is a list of =Prop=s where
            - A Prop is any of
                - type(Type) where Type is any of
                    - string
                    - integer

Example:
```
entity(person, [
    property(name, [
        type(string)
    ]),
    property(age, [
        type(integer)
    ])
]).
```
*/
:- multifile entity/2.

/** entity_property_type(?Ent, ?Prop, ?Typ)

"Entity named Ent has property named Prop whose type is named Typ."
*/
entity_property_type(E,P,T) :-
    entity(E, Props),
    member(property(P,Descs), Props),
    member(type(T), Descs).
