/** <module> Define classes as Prolog multifile predicates

Interface:

    - directive/1
    - declare_class/2

## Motivation

A class C has properties P1, P2, P3, etc.
How do we represent an _instance_ of C in Prolog?
There are at least two ways: _many-predicates_ and _one-term_.

The _many-predicates_ representation makes it easy to add derived properties.
One predicate represents one property.
This is similar to 6NF (sixth normal form) in database theory.

==
c_prop1(InstanceId, Prop1).
c_prop2(InstanceId, Prop2).
c_prop3(InstanceId, Prop3).
...
==

The _one-term_ representation makes it easy to specify an instance.
One term represents one instance.
This is similar to 0NF/1NF (zeroth or first normal form) in database theory.

==
c(InstanceId, [
    prop1 - Prop1,
    prop2 - Prop2,
    prop3 - Prop3,
    ...
]).
==

But we can combine both.
We can translate an instance-wise representation to a property-wise representation:

==
:- discontiguous c_prop1/2, ..., c_propN/2.

c_prop1(InstanceId, Prop1) :- c(InstanceId, Props), member(prop1-Prop1, Props).
c_prop2(InstanceId, Prop2) :- c(InstanceId, Props), member(prop2-Prop2, Props).
c_prop3(InstanceId, Prop3) :- c(InstanceId, Props), member(prop3-Prop3, Props).
...
==

But the many-predicates representation is easier to refactor than the one-term representation.

Conclusions:
    - A module may internally specify objects in the one-term (denormal-form) style,
    but should only export predicates in the many-predicates (normal-form) style.
    - A translation should not import denormal-form predicates.

## A note on temporal modeling

`employee_department/2` cannot keep track of movement history.
If it is important to keep track of employee movement,
we should use `employee_join_department/3` and `employee_leave_department/3`.

==
employee_department(?EmpId,?DepId)
employee_join_department(?EmpId,?DepId,?Time)
employee_leave_department(?EmpId,?DepId,?Time)
==

*/

:- discontiguous directive/1.
:- discontiguous term_expansion/2.



/** directive(?NameArity) is det.

Declare a directive.

Note: Remember to declare the corresponding term_expansion/2.

Problem: The directive has to come before the pldoc.
*/

expand_directive(Name/Arity, Clauses) :-
    functor(Head, Name, Arity),
    Clauses = [
        :- discontiguous Name/Arity
        , Head :- throw(error(dummy_predicate_called(Head),_))
    ].

term_expansion(:- directive(NameArity), Clauses) :-
    expand_directive(NameArity, Clauses),
    compile_aux_clauses(Clauses).

prolog:error_message(dummy_predicate_called(T)) -->
    ["~w calls a directive that is not meant to be actually called.~n"-[T]],
    ["Did someone forget to define the corresponding term_expansion/2?"].

:- directive(directive/1).



:- directive(declare_class/2).

/** declare_class(+ClassName,+PropList) is det.

This directive generates several multifile/1 predicates.

PropList is a list of Name/Arity.

The generated predicates have this form,
where N is the property arity (predicate arity minus one):

==
ClassName_PropertyName(?ClassId,?PropertyValue1,...,?PropertyValueN) is nondet.
==

Example:

==
% This directive ...

:- declare_class(employee, [name/1, date_lunch/2])

% ... generates these predicates:

% employee(?EmployeeId) is nondet.
% employee_name(?EmployeeId, ?Name) is nondet.
% employee_date_lunch(?EmployeeId, ?Date, ?Lunch) is nondet.

:- multifile employee/1.
:- multifile employee_name/2.
:- multifile employee_date_lunch/3.
==

The primary key ClassId exists because _equal_ objects are not necessarily _identical_ objects.
Two objects are considered identical iff they have the same class and they have the same primary key.

Terminology: An _object_ has a _class_, an _identity_, and _properties_.
*/

term_expansion(:- declare_class(Class,Props), Expansion) :-
    expand_class(Class, Props, Expansion).

expand_class(Class, Props, Clauses) :-
    expand_class_1(Class, Clauses1),
    findall(
        Clauses2,
        (   member(PropName/PropArity, Props),
            expand_class_property(Class, PropName/PropArity, Clauses2)
        ),
        Clauses2s
    ),
    append([Clauses1|Clauses2s], Clauses).

expand_class_1(Class, [:- multifile Class/1]).

expand_class_property(Class, PropName/PropArity, [:- multifile PredName/PredArity]) :-
    PredArity is PropArity + 1,
    atomic_list_concat([Class,'_',PropName], PredName).
