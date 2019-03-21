/** <module> Define classes as Prolog multifile predicates

Interface:

    - directive/1
    - declare_class/2
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
