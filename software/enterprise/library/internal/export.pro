% -------------------- wiring: presenting internal structure as schema/java_program.pro structure

element_access(A,public) :- element_option(A,public), !.
element_access(A,protected) :- element_option(A,protected), !.
element_access(A,package) :- element_option(A,package), !.
element_access(A,private) :- element_option(A,private), !.
element_access(_,public).

element_static(A,true) :- element_option(A,static), !.
element_static(_,false).

element_final(A,true) :- element_option(A,final), !.
element_final(_,false).

element_annotation(_, _, _) :- fail. % TODO

    element_options(A,B) :- java_class(A,_,_,B).
    element_options(A,B) :- java_class_constructor(_,A,_,B).
    element_options(A,B) :- java_class_method(_,A,_,_,_,B).
    element_options(A,B) :- java_class_field(_,A,_,_,B).

    element_option(A,B) :- element_options(A,L), member(B,L).

class(C) :- java_class(C,_,_,_).
class_constructor(C,K) :- java_class_constructor(C,K,_,_).
class_package_name(A,B) :- java_class(A,B,_,_).
class_name(A,B) :- java_class(A,_,B,_).
class_comment(C,K) :- java_class(C,_,_,L), member(comment-K,L).
class_extend(C,S) :- element_option(C, extend-A) -> S = some(A) ; S = none.
class_implements(C,L) :- element_option(C, implements-List) -> L = List ; L = [].
class_implement(C,I) :- class_implements(C,L), member(I,L).

class_field(A,B) :- java_class_field(A,B,_,_,_).
class_method(A,B) :- java_class_method(A,B,_,_,_,_).

field(A) :- java_class_field(_,A,_,_,_).
field_name(A,B) :- java_class_field(_,A,_,B,_).
field_type(A,B) :- java_class_field(_,A,B,_,_).
field_initializer(A,B) :-
    java_class_field(_,A,_,_,L),
    (member(initializer-I,L)
    ->  B = some(I)
    ;   B = none).

constructor(A) :- java_class_constructor(_,A,_,_).
method(A) :- java_class_method(_,A,_,_,_,_).
method_name(A,B) :- java_class_method(_,A,_,B,_,_).
method_return_type(A,B) :- java_class_method(_,A,B,_,_,_).

/** callable_parameter(?CallableId,?Order,?ParamId,?ParamType,?ParamName,?ParamOpts)

callable_parameter/6 is the most convenient way to specify a parameter.
*/

:- multifile callable_parameter/6.

parameter_name(Param,Name) :- callable_parameter(_,_,Param,_,Name,_).
parameter_type(Param,Type) :- callable_parameter(_,_,Param,Type,_,_).

% callable_parameter/5 is deprecated; use callable_parameter/6 instead
callable_parameter(Method,N,Method-N,Type,Name) :-
    java_class_method(_,Method,_,_,Params,_),
    nth1(I,Params,[Type,Name|_]),
    N is 100*I.

callable_parameter(Ctor,N,Ctor-N,Type,Name) :-
    java_class_constructor(_,Ctor,Params,_),
    nth1(I,Params,[Type,Name|_]),
    N is 100*I.

:- multifile parameter_name/2,
             parameter_type/2.
callable_parameter(C,N,P) :- callable_parameter(C,N,P,_,_).
callable_parameter(C,N,P) :- callable_parameter(C,N,P,_,_,_).
parameter_type(Param,Type) :- callable_parameter(_,_,Param,Type,_).
parameter_name(Param,Name) :- callable_parameter(_,_,Param,_,Name).

callable_options(Id,Opts) :- java_class_constructor(_,Id,_,Opts).
callable_options(Id,Opts) :- java_class_method(_,Id,_,_,_,Opts).

/** callable_statements(?CallableId,?BaseOrder,?Statements) is nondet.

callable_statements/3 is the most convenient way to declare callable statements.

This predicate is multifile so that we can insert aspects.
*/
:- multifile callable_statements/3.

:- multifile callable_statement/3.
callable_statement(Callable,Order,Stmt) :-
    callable_statements(Callable,BaseOrder,Stmts),
    nth0(N,Stmts,Stmt),
    Order is BaseOrder + N.
callable_statement(Callable,Order,Stmt) :-
    callable_options(Callable,Opts),
    member(body-Stmts,Opts),
    statements_(Order,Stmt,100,Stmts).

    % statements_(-Order,-Stmt,+BaseOrder,+List).
    % Helper for making choice points from a list.
    statements_(Order,Stmt,BaseOrder,List) :-
        nth0(N,List,Stmt),
        Order is BaseOrder + N.

callable_throws(Method,Throws) :-
    element_option(Method,throws-Throws) -> true ; Throws = [].

callable_throw(Method,Throw) :-
    callable_throws(Method,Throws),
    member(Throw,Throws).
