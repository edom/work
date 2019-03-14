:- module(pal_interpreter,[
    interpret_procedure/1
]).

/** <module> Procedure Action Language

See also pal_usage.md and pal_design.md.
*/



% -------------------- syntax-checking



/** expression(-Term) is semidet.

Term is an expression.

Syntax-checking predicate.
*/

expression(A) :- var(A), !, instantiation_error(A).
expression(A+B) :- expression(A), expression(B).
expression(A->B) :- atom(A), expression(B).
expression((A:_)->B) :- atom(A), expression(B).
expression(E:_) :- expression(E).
expression(append(A,B)) :- expression(A), expression(B).
expression(record(L)) :- is_record_syntax(L).
expression(A) :- atom(A).
expression(A) :- number(A).
expression(A) :- string(A).

is_record_syntax([]).
is_record_syntax([A|Z]) :- A = Name-E, atom(Name), expression(E), is_record_syntax(Z).



% -------------------- type-checking



/** exp_checked_type(+UncheckedExp, --CheckedExp, --Type) is det.

Decorate UncheckedExp with types and check those types.

UncheckedExp must not contain free variables.

Throw an exception on type-checking failure.
*/

exp_checked_type(E, C, T) :- typecheck([], E, C, T).



% typecheck(+Context, +UncheckedExp, --CheckedExp, --Type) is det.

typecheck(_, E, _, _) :- \+ expression(E), !, domain_error(expression, E).
typecheck(_, _, C, _) :- \+ var(C), !, type_error(var,C).
typecheck(_, _, _, T) :- \+ var(T), !, type_error(var,T).
typecheck(G, A:B, CA, T) :- !, typecheck(G,A,CA,T), expected_actual(T,B).

typecheck(G, A->B, C, T) :- !,
    T = (TA->TB),
    C = (CA->CB):T,
    typecheck(G, A, CA, TA),
    typecheck([CA|G], B, CB, TB).

typecheck(G, A+B, C, T) :- !,
    C = (CA+CB):T,
    typecheck(G, A, CA, TA),
    typecheck(G, B ,CB, TB),
    type_add(TA, TB, T).

typecheck(G, append(A,B), C, T) :- !,
    C = append(CA,CB):T,
    typecheck(G, A, CA, TA),
    typecheck(G, B ,CB, TB),
    expected_actual(string, TA),
    expected_actual(string, TB),
    expected_actual(string, T).

typecheck(G, record(Pairs), C, T) :- !,
    C = record(C0):T,
    T = record(T0),
    typecheck_record(G, Pairs, C0, T0).

% bound variable

typecheck(G, E, C, T) :- atom(E), member(E:S,G), !, expected_actual(S,T), C = E:T.

% free variable

typecheck(_, E, C, T) :- atom(E), !, C = E:T.

typecheck(G, E, C, T) :- typecheck_numeric(G, E, C, T).
typecheck(_, E, C, T) :- string(E), !, C = E:T, T = string.



expected_actual(E,A) :- E = A, !.
expected_actual(E,A) :- !, throw(error(expected_actual(E,A),_)).



typecheck_record(_, [], C, T) :- C = [], T = [].
typecheck_record(G, [Name-Exp|Z], C, T) :-
    C = [Name-CExp|CZ],
    T = [Name-TA|TZ],
    typecheck(G, Exp, CExp, TA),
    typecheck_record(G, Z, CZ, TZ).



typecheck_numeric(_, E, C, T) :- natural(E), C = E:T, T = natural.
typecheck_numeric(_, E, C, T) :- integer(E), C = E:T, T = integer.
typecheck_numeric(_, E, C, T) :- rational(E), C = E:T, T = rational.
typecheck_numeric(_, E, C, T) :- float(E), C = R:T, R is rational(E), T = rational.

natural(A) :- integer(A), A >= 0.



type_add(A, B, C) :- \+ numeric_type(A), !, throw(error(type_add(A,B,C),_)).
type_add(A, B, C) :- \+ numeric_type(B), !, throw(error(type_add(A,B,C),_)).
type_add(A, B, C) :- \+ numeric_type(C), !, throw(error(type_add(A,B,C),_)).
type_add(A, B, C) :-
    numeric_type(A),
    numeric_type(B),
    numeric_type(C),
    subtype(A, C),
    subtype(B, C).

numeric_type(natural).
numeric_type(integer).
numeric_type(rational).



subtype(A, A).
subtype(A, C) :- direct_subtype(A, B), subtype(B, C).

direct_subtype(natural, integer).
direct_subtype(integer, rational).



% ------- procedure interpreter, text user interface



/** interpret_procedure(+ProcId) is det.

Interpret.
*/

:- dynamic tmp_input/2,
           tmp_output/2.

interpret_procedure(Proc) :-
    (procedure_name(Proc, Name) -> true ; throw(procedure_unknown(Proc))),
    (procedure_action(Proc, Statement) -> true ; throw(procedure_no_statement(Proc))),
    format("Interpreting procedure ~w (~w)~n", [Proc,Name]),
    retractall(tmp_input(_,_)),
    forall(
        procedure_input(Proc,InputName,Type),
        (
            read_input(Type,InputName,Input),
            assertz(tmp_input(InputName,Input))
        )
    ),
    interpret_statement(Statement),
    forall(
        procedure_output(Proc,OutputName,_),
        (
            tmp_output(OutputName,OutputValue),
            format("Output ~w is ~w~n", [OutputName,OutputValue])
        )
    ).

interpret_statement([]) :- !.
interpret_statement([A|B]) :- !, interpret_statement(A), interpret_statement(B).
interpret_statement(Out:=Exp) :- !,
    interpret_expression(Exp,Val),
    retractall(tmp_output(Out,_)),
    assertz(tmp_output(Out,Val)).
interpret_statement(if(Cond,Act)) :- interpret_condition(Cond) -> interpret_statement(Act) ; true.
interpret_statement(fail(Msg)) :- format("fail: ~w~n",[Msg]), throw(error(fail(Msg),_)).
interpret_statement(Statement) :- domain_error(statement,Statement).

interpret_expression(A,Z) :- atom(A), !, (tmp_input(A,Z) -> true ; throw(error(undefined_input(A),_))).
interpret_expression(A,Z) :- number(A), !, Z = A.
interpret_expression(A+B,Z) :- !, interpret_expression(A,A0), interpret_expression(B,B0), Z is A0+B0.
interpret_expression(A*B,Z) :- !, interpret_expression(A,A0), interpret_expression(B,B0), Z is A0*B0.
interpret_expression(A>=B,Z) :- !, interpret_expression(A,A0), interpret_expression(B,B0), (A0 >= B0 -> Z = true ; Z = false).
interpret_expression(A,_) :- domain_error(expression,A).

interpret_condition(A) :- interpret_expression(A,B), from_boolean(B).
    from_boolean(A) :- var(A), instantiation_error(A).
    from_boolean(true) :- !.
    from_boolean(false) :- !, false.
    from_boolean(A) :- domain_error(boolean,A).

read_input(natural,Input,Result) :- !,
    format("Reading natural number ~w:~n",[Input]),
    read(A),
    (integer(A), A >= 0 -> Result = A ; read_input(natural,Input,Result)).
read_input(Type,Input,_) :-
    format("Don't know how to read input ~w of type ~w~n", [Input,Type]),
    throw(error(input(Input,Type),_)).
