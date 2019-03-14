:- module(prolog_component,[

    % -------------------- directives

    declare_plug/2
    , declare_socket/2

    % -------------------- actions

    , connect_plug_to_socket/2
    , connect_plug_to_socket/3

    , compile_aux_clauses_0/1
]).

/** <module> Socket-plug metaphor for programming-in-the-large

A pin is a Name/Arity term.

A plug exports symbols.

A socket imports symbols.

A module may have multiple plugs and sockets.

Pins are matched by NameArity.
The ordering of pins does not matter.

For module authors:

    - Use the declare_plug/2 and declare_socket/2 directives.

For module users:

    - Connect same-named pins with connect_plug_to_socket/2.
    If manual pin connection is required due to name clash, use connect_plug_to_socket/3.
*/



% -------------------- deconstruction



plug_module_name(Module:Name, A, B) :- !, A = Module, B = Name.
plug_module_name(A, _, _) :- type_error(plug,A).

socket_module_name(Module:Name, A, B) :- !, A = Module, B = Name.
socket_module_name(A, _, _) :- type_error(socket,A).



/** declare_plug(+Name,+Preds) is det.
    declare_socket(+Name,+Preds) is det.
*/

declare_plug(PlugName, Preds) :-
    compile_aux_clauses_0(['$plug'(PlugName,Preds)]).

declare_socket(SocketName, Preds) :-
    Facts = ['$socket'(SocketName,Preds)],
    findall(Clause,
        (   member(Pred,Preds),
            declare_socket_pin(SocketName,Pred,Clause)
        ),
        ListList),
    append(ListList, Multifiles),
    append([Facts,Multifiles], Clauses),
    compile_aux_clauses_0(Clauses).

declare_socket_pin(_, Pred, [:- multifile Pred]).

term_expansion(:- declare_socket(Name,Pins), do_declare_socket(Name,Pins)).



/** '$socket'(?SocketName,?SocketPins) is nondet.
    '$plug'(?PlugName,?PlugPins) is nondet.

Usage:
    - Define `$plug` in the module that exports the symbol.
    - Define `$socket` in the module that imports the symbol.
*/
plug_pins(Plug, Pins) :-
    plug_module_name(Plug, Module, PlugName),
    get_module_connector_pins(plug, Module, PlugName, Pins).

socket_pins(Socket, Pins) :-
    socket_module_name(Socket, Module, SocketName),
    get_module_connector_pins(socket, Module, SocketName, Pins).

get_module_connector_pins(_, Module, _, _) :-
    \+ ground(Module),
    !, instantiation_error(Module).

get_module_connector_pins(Type, Module, Name, Pins) :-
    \+ var(Name),
    atom_concat('$', Type, Pred),
    \+ call(Module:Pred, Name, Pins),
    !, throw(no_such(Type,Module:Name)).

get_module_connector_pins(Type, Module, Name, Pins) :-
    atom_concat('$', Type, Pred),
    call(Module:Pred, Name, Pins).

plug_pin(Plug, Pin) :-
    plug_pins(Plug, Pins),
    member_0(plug, Plug, Pin, Pins).

socket_pin(Socket, Pin) :-
    socket_pins(Socket, Pins),
    member_0(socket, Socket, Pin, Pins).

member_0(Type, Obj, Elem, List) :-
    nonvar(Elem), \+ member(Elem, List), !,
    throw(error(no_such_pin(Type,Obj,Elem),_)).

member_0(_, _, Elem, List) :- member(Elem, List).



/** connect_plug_to_socket(+SourcePlug,+TargetSocket) is det.
    connect_plug_to_socket(+SourcePlug,+TargetSocket,+Mapping) is det.

This must be run at compile time (such as from a directive), not at run time.
*/

connect_plug_to_socket(Plug, Socket) :-
    findall(Mapping,
        plug_socket_mapping_clause(Plug,Socket,Mapping,_),
        Mappings
    ),
    connect_plug_to_socket(Plug, Socket, Mappings).

connect_plug_to_socket(Plug, Socket, Mappings) :-

    % error-checking

    plug_pins(Plug, PlugPins),
    socket_pins(Socket, SocketPins),
    subtract(PlugPins, SocketPins, PlugOnly),
    subtract(SocketPins, PlugPins, SocketOnly),
    ((PlugOnly == [], SocketOnly == [])
        ->  true
        ;   throw(error(connect_shape_mismatch(Plug,Socket,PlugOnly,SocketOnly)))),

    % happy path

    findall(Clause,
        (   member(Mapping,Mappings),
            plug_socket_mapping_clause(Plug,Socket,Mapping,Clause)
        ),
        Clauses
    ),
    compile_aux_clauses_0(Clauses).

prolog:error_message(connect_shape_mismatch(SourcePlug,TargetSocket,PlugOnly,SocketOnly)) -->
    ["Shape mismatch: cannot connect plug ~w to socket ~w\n"-[SourcePlug,TargetSocket]],
    ["Pins only in plug: ~w\n"-PlugOnly],
    ["Pins only in socket: ~w"-SocketOnly].

%% plug_socket_mapping_clause(+Plug,+Socket,+Mapping,-Clause) is det.

plug_socket_mapping_clause(Plug, Socket, Mapping, Clause) :-
    plug_module_name(Plug, Source, _),
    socket_module_name(Socket, Target, _),
    Mapping = SName/Arity - TName,
    plug_pin(Plug, SName/Arity),
    socket_pin(Socket, TName/Arity),
    Clause = (Target:THead :- Source:SHead),
    length(Args, Arity),
    SHead =.. [SName|Args],
    THead =.. [TName|Args].



/** compile_aux_clauses_0(+Clauses) is det.

This is compile_aux_clauses/1 that fails loudly.

compile_aux_clauses/1 only works at compile-time (such as from a directive),
and it fails silently if that is not the case.
*/

compile_aux_clauses_0(Clauses) :-
    compile_aux_clauses(Clauses)
    ->  true
    ;   throw(error(too_late_to_run(compile_aux_clauses_0/1),_)).

prolog:error_message(too_late_to_run(Pred)) -->
    ["It is too late to run ~w.~n"-[Pred]],
    ["It can only run at compile-time, not at run-time.~n"],
    ["Hint: Directives run at compile-time."].
