% -------------------- types



type_definition(currency-identifier, #identifier).
type_definition(currency-name, #string).
type_definition(currency-symbol, #string).
type_definition(currency, #record([
    id : currency-identifier
    , symbol : currency-symbol
    , name : #optional(currency-name)
])).
type_definition(account-identifier, #identifier).
type_definition(account-name, #string).
type_definition(account, #record([
    id : account-identifier
    , name : account-name
])).



% ------- unused sketch



state_definition(bag-account, [type-bag(account)]).

autocrud(account).
autocrud(currency).

% constraint(forall(Type, autocrud(Type) -> type(Type-identifier))).



% -------------------- procedures



procedure_definition(trivial,[
    inputs-[x:natural, y:natural]
    , output-record([
        sum-(x + y)
        , product-(x * y)
    ])
    , checks-[x < 100, y < 100]
    , name-"some trivial computations"
]).

/*
procedure_definition(find-Type-by-id,[
    name-Name
    , inputs-[id : Type-identifier]
    , outputs-[result : #optional(Type)]
    , action-[
        output(answer) := find(state(bag-Type), [id = input(id)])
    ]
]) :-
    autocrud(Type),
    format(string(Name), "Find ~w by id", [Type]).
*/

procedure(Id) :- procedure_definition(Id,_).
procedure_property(Id,K,V) :- procedure_definition(Id,L), member(K-V,L).
procedure_name(A,B) :- procedure_property(A,name,B).
procedure_check(Proc,Check) :- procedure_property(Proc,checks,Checks), member(Check, Checks).
procedure_input(Proc,Name,Type) :- procedure_property(Proc,inputs,Inputs), member(Name:Type,Inputs).
procedure_output(Proc,Output) :- procedure_property(Proc,output,Output).



% ------- implementation details



type_maxbitcount(account-identifier, 32).
type_maxbitcount(currency-identifier, 16).
type_maxbytecount(currency-symbol, 8).
type_maxbytecount(currency-name, 128).
type_maxbytecount(account-name, 128).

type_primarykey(currency, [id]).
type_primarykey(account, [id]).



% ------- web application



state(S) :- state_type(S,_).
state_name(S, S) :- state(S).
state_type(lastvalue, #string).
state_initializer(lastvalue, "").

thing_property(database,Id,Key,Value) :- database(Id,Props), member(Key-Value,Props).
thing_property(table,Id,Key,Value) :- table(Id,Props), member(Key-Value,Props).

/*
sudo -u postgres psql
    CREATE USER test PASSWORD 'test';
    CREATE DATABASE test OWNER test;
*/
database(accounting,[
    name-accounting
    , host-localhost
    , port-5432
    , catalog-test
    , username-test
    , password-test
]).
database_property(D,Key,Value) :- thing_property(database,D,Key,Value).
database(D) :- database(D,_).
database_name(A,B) :- database_property(A,name,B).
database_host(A,B) :- database_property(A,host,B).
database_port(A,B) :- database_property(A,port,B).
database_catalog(A,B) :- database_property(A,catalog,B).
database_username(A,B) :- database_property(A,username,B).
database_password(A,B) :- database_property(A,password,B).

table(account,[
    database-accounting
    , schema-account
    , name-account
]).

table(A) :- table(A,_).
table_database(A,B) :- thing_property(table,A,database,B).
table_schema(A,B) :- thing_property(table,A,schema,B).
table_name(A,B) :- thing_property(table,A,name,B).

/** page(?PageId,?Name,?HttpMethod,?Path,?PageOpts,?Content) is nondet.

A convenience predicate for specifying pages.
*/
page(get-home, home, 'GET', '/', [], [
    let([value = request_parameter(value)],[
        "The last value was ", state(lastvalue),
        state(lastvalue) := value
    ])
]).

/*
We could define our own operators, but should we?

Pro: The code looks pretty.

Con: The syntax error message will always be "Syntax error: Operator priority clash".

:- op(1140,xfx,limit).
:- op(1130,xfx,order_by).
:- op(1120,xfx,where).
:- op(1110,yfx,join).
% :- op(700,yfx,as).
:- write_canonical(
    $table1 as t1 join $table2 as t2 join $table3 as t3
    where [t1:field1=t2:field2, t2:field2=t3:field3]
    order_by [t1:field1, t2:field2]
    limit 100
).
*/

page(get-help, help, 'GET', '/help', [], [
    "Help"
    , query(limit(15,order_by([id,name],$account)))
]).

page(post-help, help, 'POST', '/help', [], [
    "Help (POST)"
]).

% ------- wiring

page(P) :- page(P,_,_,_,_,_).
page_name(P,N) :- page(P,N,_,_,_,_).
page_method(P,M) :- page(P,_,M,_,_,_).
page_path(P,A) :- page(P,_,_,A,_,_).
page_options(P,A) :- page(P,_,_,_,A,_).
page_content(P,C) :- page(P,_,_,_,_,C).
page_option(P,A) :- page_options(P,L), member(A,L).

% -------------------- globalization

term_locale_string(hello, english, "Hello.").

% -------------------- code generation parameters

base_package_name("com.spacetimecat.java").
maven_coordinates(GroupId, "accounting", "0.0.0") :- base_package_name(GroupId).
output_dir("out").
dry_run(false).

% ------- dreams



/** inline(+Pattern,+Term,-Replacement) is nondet.

This can be used to enumerate possible partial deduction with respect to the currently loaded predicates.

Constraint: =call(Pattern)= should only succeed a finite number of times.

Example:

==
p(A) :- q(A), r(A).

q(A) :- r(B), A is B + 100.

r(0).
r(1).

?- inline(q(_), (p(A) :- q(A),r(A)), Z), write(Z), nl, fail.

% produces:

p(100):-true,r(100)
p(101):-true,r(101)
==

How this may be useful:

    1. Write a naive interpreter for the Prolog terms representing the abstract syntax of a language.
    2. Use partial deduction to derive a Prolog program equivalent to the interpreter but much faster.

Problem: This inline/3 does not work for recursive predicates:

==
interpret(A,Z) :- number(A), !, Z = A.
interpret(A+B,Z) :- Z is A+B.
interpret(A*B,Z) :- Z is A*B.

?- inline(interpret(_), (read(A), interpret(A,B), write(B), nl), Program).
==

Maybe we shouldn't use call/1.

*/
inline(Pattern, (A:-B), Z) :- !, Z = (A:-C), inline(Pattern, B, C).
inline(Pattern, (A,B), (Y,Z)) :- !, inline(Pattern,A,Y), inline(Pattern,B,Z).
inline(Pattern, (A;B), (Y;Z)) :- !, inline(Pattern,A,Y), inline(Pattern,B,Z).
inline(Pattern, (A->B), (Y->Z)) :- !, inline(Pattern,A,Y), inline(Pattern,B,Z).
inline(Pattern, Term, true) :- unifiable(Pattern,Term,_), copy_term(Pattern,Term), call(Term).
inline(_, Term, Term).

% ------- Dream: Translate Prolog/Datalog to SQL.

:- dynamic employee/1,
           employee_name/2,
           employee_join_date/2.

% Dream: Translate employee/1 and its properties to database query.
dream_query((
    employee_old_timer(E) :-
        now(Now),
        employee_join_date(E,J),
        subtract_date(Now,J,Duration),
        Duration >= duration(10,year)
)).

dream_query((
    engineer(E) :-
        employee_department(E,D),
        department_name(D,"Engineering")
)).
