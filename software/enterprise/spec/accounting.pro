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

usecase_definition(ask, (
    input(#natural,Value),
    display(Value)
)).

usecase_definition(create-Type, (
    input(Type, Value),
    insert1(state(bag-Type), Value)
)) :- autocrud(Type).

% usecase is harder to model than business process?

/*
A business process (an atomic interaction? a transaction?) has inputs, action, and outputs.
The action is run after all inputs are ready.
The outputs are set after the action has finished.
If an input is not ready, we expect the system to ask the user for that input.
*/

process_definition(ask, [
    inputs([number : #natural]),
    outputs([answer : #natural]),
    action(answer := number + 1)
]).

process_definition(find-Type-by-id, [
    inputs([id : Type-identifier]),
    outputs([result : #optional(Type)]),
    action([
        output(answer) := find(state(bag-Type), [id = input(id)])
    ])
]) :- autocrud(Type).

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
state_initializer(lastvalue, some("")).

/** page(?PageId,?Name,?HttpMethod,?Path,?PageOpts,?Content) is nondet.

A convenience predicate for specifying pages.
*/
page(get-home, home, 'GET', '/', [], [
    let([value = request_parameter(value)],[
        "The last value was ", state(lastvalue),
        state(lastvalue) := value
    ])
]).

page(get-help, help, 'GET', '/help', [], [
    "Help"
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
