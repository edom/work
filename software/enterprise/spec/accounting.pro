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

webapp(accounting).
webapp_page(accounting,P) :- page_path(P,_).
webapp_state(accounting,accounting-S) :- state(accounting-S).

state(accounting-S) :- state_type(accounting-S,_).
state_type(accounting-lastvalue,#string).
state_initializer(accounting-lastvalue,"").

% page(accounting-home, get, '/', [], "Hello").

page_method(accounting-home,get).
page_path(accounting-home,'/').
page_content(accounting-home,[
    let([value = request_parameter(value)],[
        "The last value was ", state(accounting-lastvalue),
        state(lastvalue) := value
    ])
]).

page_method(accounting-help,get).
page_method(accounting-help,post).
page_path(accounting-help,'/help').
page_content(accounting-help,[
    "Help"
]).
