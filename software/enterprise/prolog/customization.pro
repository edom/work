:- module(prolog_customization,[
    throw_error/1
]).

/** <module> Tailoring Prolog to our requirements
*/

throw_error(E) :- throw(error(E,_)).
