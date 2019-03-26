:- op(1,fx,#).
:- op(50,yfx,@).

:- discontiguous
    object_proplist/2.

/*
We have two options; to mix metadata, or to separate metadata.

Mixed:
opv(thing, #instance_of, #class).
opv(thing, #have, name).
opv(my_thing, #instance_of, thing).
opv(my_thing, name, thingie).

Separated:
class(thing).
class_property(thing, name).
instance_of(my_thing, thing).
opv(my_thing, name, thingie).
*/

opv(O, P, V) :- object_proplist(O, PL), member(P-V, PL).

% "C #have P" means that each instance of C has a property named P.
opv(C, #have, P) :- opv(#class_property(C,P), #type, _).

opv(thing, #instance_of, #class).
opv(thing, #subclass_of, #object).
opv(thing, #have, height).

opv(person, #instance_of, #class).
opv(person, #subclass_of, thing).
opv(#class_property(person,name), #type, #string).
opv(person, #have, role).
opv(person, #have, birth_date).

opv(driver, #instance_of, #class).
opv(driver, #subclass_of, person).
opv(#class_property(driver,car), #type, car).

opv(car, #instance_of, #class).
opv(car, #subclass_of, thing).
opv(#class_property(car,color), #type, color).
opv(#class_property(car,buy_year), #type, #integer).

opv(color, #instance_of, #class).
opv(color, #subclass_of, #object).

opv(black, #instance_of, color).

opv(white, #instance_of, color).

opv(alice, #instance_of, driver).
opv(alice, name, invalid_name).

opv(bob, #instance_of, person).

opv(jack, #instance_of, driver).
opv(jack, name, "Jack ABC").
opv(jack, car, not_a_car).

opv(john, #instance_of, driver).
opv(john, name, "John Doe").
opv(john, role, bystander).
opv(john, birth_date, date(1980,1,1)).
opv(john, height, 175-cm).
opv(john, car, car_of_john).

opv(car_of_john, #instance_of, car).
opv(car_of_john, color, black).
opv(car_of_john, buy_year, 2000).
opv(car_of_john, height, 200-cm).

object_proplist(borg, [
    #instance_of - person
    , name-"Borg"
    , birth_date-date(1000,1,1)
]).

relation(know).
relation_arity(know, 2).
instance_relation([john,car_of_john], know).

%%  interpret(+Exp,-Result) is det.
%   Name@Args means apply (add arguments Args to the functor term Name).
%   Example: interpret(a@[1,2], a(1,2)).
%   Example: interpret(a(1,2)@[3,4,5], a(1,2,3,4,5)).
interpret(Name@Args, Z) :- !,
    interpret(Name, IName),
    interpret(Args, LArgs),
    IName =.. LName,
    append(LName, LArgs, LFunctor),
    Functor =.. LFunctor,
    interpret(Functor, Z).

interpret(A+B, Z) :- !, interpret(A,A0), interpret(B,B0), atom_concat(A0,B0,Z).
interpret(A/B, Z) :- !, interpret(A,A0), interpret(B,B0), Z = A0/B0.
interpret(require(A), Z) :- !, interpret(A,A0), Z = (:- multifile(A0)).
interpret([A|B], Z) :- !, Z = [A0|B0], interpret(A,A0), interpret(B,B0).
interpret(A, Z) :- !, Z = A.

instance_of(A, #integer) :- integer(A).
instance_of(A, #string) :- string(A).
instance_of(I, C) :- opv(I, #instance_of, C).
instance_of(I, P) :- subclass_parent(C,P), instance_of(I,C).

object(A) :- instance_of(A, #object).
class(A) :- instance_of(A, #class).

class_property(A, B) :-
    instance_of(A, #class),
    opv(A, #have, B).

class_property_type(C, P, T) :-
    opv(#class_property(C,P), #type, T).

subclass_parent(A, B) :- opv(A, #subclass_of, B).

generated_clause(Clause) :- gen(A), interpret(A, Clause).

gen(require((C+'_'+P)/2)) :- class(C), class_property(C,P).
gen(require(Rel/Arity)) :- relation(Rel), relation_arity(Rel,Arity).
gen(C@[I]) :- class(C), instance_of(I,C).
gen(R@I) :- relation(R), instance_relation(I,R).
gen((C+'_'+P)@[I,V]) :- class(C), class_property(C,P), instance_of(I,C), opv(I,P,V).

check_ontology :-
    forall(
        instance_missing_property(I,P,C),
        format("Error: Missing property: object \e[1m~w\e[22m, property \e[1m~w\e[22m, required by class \e[1m~w\e[22m\n",[I,P,C])
    ),
    forall(
        instance_property_wrong_type(I,P,V,T),
        format("Error: Property type error: object \e[1m~w\e[22m, property \e[1m~w\e[22m, value \e[1m~w\e[22m, should be of type \e[1m~w\e[22m\n",[I,P,V,T])
    ).

instance_missing_property(I, P, C) :-
    object(I),
    instance_of(I, C),
    class_property(C, P),
    \+ opv(I, P, _).

instance_property_wrong_type(I, P, V, T) :-
    object(I),
    instance_of(I, C),
    class_property_type(C, P, T),
    opv(I, P, V),
    \+ instance_of(V, T).

