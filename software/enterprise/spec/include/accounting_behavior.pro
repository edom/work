% -------------------- state

state(S) :- state_type(S,_).
state_name(S, S) :- state(S).
state_type(lastvalue, #string).
state_initializer(lastvalue, "").

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
