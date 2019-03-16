:- module(prolog_component,[

    % -------------------- directives

    declare_plug/2
    , declare_socket/2

    % -------------------- actions

    , connect_plug_to_socket/2
    , connect_plug_to_socket/3

    , compile_aux_clauses_0/1
]).

:- use_module(library(debug)).

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

Things:

    - A _qcon_ (qualified connector name) is a Module:Name term.
    - A _pin_ is a Name/Arity term.

Debug topics for library(debug):

    - `connector`: declarations, connections
*/



% -------------------- tracing



debug_connector(Format, Args) :-
    context_module(Module),
    format(string(Format0), "~w: ~w", [Module,Format]),
    debug(connector, Format0, Args).



% -------------------- deconstruction



qcon_module_name(Module:Name, A, B) :- !, A = Module, B = Name.
qcon_module_name(Con, _, _) :- type_error(qualified_connector_name,Con).



/** connector(?Connector) is nondet.

Type is `plug` or `socket`.

QualConName is Module:ConName.

Pins is a list of Name/Arity.

This is less invasive than dynamically declaring predicates in the Module.

This predicate is a private implementation detail.
*/

:- multifile connector/1.

declare_connector(Con) :-
    debug_connector("Declaring ~w", [Con]),
    (must_be(ground,Con), term_is_connector(Con) ; type_error(connector,Con)), !,
    compile_aux_clauses_0([
        prolog_component:connector(Con)
    ]).

term_is_connector(A) :- functor(A, connector, 4).
term_connector_type(A,B) :- term_is_connector(A), arg(1,A,B).
term_connector_module(A,B) :- term_is_connector(A), arg(2,A,B).
term_connector_name(A,B) :- term_is_connector(A), arg(3,A,B).
term_connector_pins(A,B) :- term_is_connector(A), arg(4,A,B).



% -------------------- directives



/** declare_plug(+QualName,+Preds) is det.
    declare_socket(+QualName,+Preds) is det.
*/

:- meta_predicate declare_plug(:,+),
                  declare_socket(:,+).

declare_plug(Qcon, Preds) :-
    qcon_module_name(Qcon, Module, Name),
    declare_connector(connector(plug,Module,Name,Preds)).

declare_socket(Qcon, Preds) :-
    qcon_module_name(Qcon, Module, Name),
    declare_connector(connector(socket,Module,Name,Preds)),
    forall(member(Pred,Preds), Module:multifile(Module:Pred)).



% -------------------- getters that throw on fail



get_connector_pins(Type, Qcon, Pins) :-

    must_be(ground,Type),
    \+ (\+ member(Type,[plug,socket]), domain_error(connector_type,Type)),

    must_be(ground,Qcon),
    qcon_module_name(Qcon, Module, Name),

    Con = connector(Type, Module, Name, Pins),

    \+ (\+ connector(Con), throw_error(no_such(Type,Qcon))),

    connector(Con).



get_connector_pin(Type, Qcon, Pin) :-
    get_connector_pins(Type, Qcon, Pins),
    member_0(Type, Qcon, Pin, Pins).

member_0(Type, Obj, Elem, List) :-
    nonvar(Elem), \+ member(Elem, List), !,
    throw_error(no_such_pin(Type,Obj,Elem)).

member_0(_, _, Elem, List) :- member(Elem, List).



% -------------------- connection



/** connected(?Plug,?Socket) is nondet.

Internal connected table.
*/

:- multifile connected/2.

/** connect_plug_to_socket(+SourcePlug,+TargetSocket) is det.
    connect_plug_to_socket(+SourcePlug,+TargetSocket,+Mapping) is det.

This must be run at compile time (such as from a directive), not at run time.

Mapping is `SName/SArity - TName/TArity`.
*/

connect_plug_to_socket(Plug, Socket) :-
    get_connector_pins(plug, Plug, PlugPins),
    findall(Pin-Pin,
        member(Pin,PlugPins),
        Mappings
    ),
    connect_plug_to_socket(Plug, Socket, Mappings).

connect_plug_to_socket(Plug, Socket, Mappings) :-

    \+ (connected(Plug,Socket), throw_error(already_connected(Plug,Socket))),

    debug_connector("Connecting plug ~w to socket ~w with mapping ~w", [Plug,Socket,Mappings]),

    % error-checking

    get_connector_pins(plug, Plug, PlugPins),
    get_connector_pins(socket, Socket, SocketPins),
    subtract(PlugPins, SocketPins, PlugOnly),
    subtract(SocketPins, PlugPins, SocketOnly),
    ((PlugOnly == [], SocketOnly == [])
        ->  true
        ;   throw_error(connect_shape_mismatch(Plug,Socket,PlugOnly,SocketOnly))),

    (forall(member(Pin,PlugPins),
        (member(Pin-_, Mappings) -> true ; throw_error(unmapped_pin(Plug,Pin)))
    )),

    % happy path

    findall(Clause,
        (   member(Mapping,Mappings),
            plug_socket_mapping_clause(Plug,Socket,Mapping,Clause)
        ),
        Clauses
    ),
    Connect = prolog_component:connected(Plug,Socket),
    Compile = [Connect|Clauses],
    compile_aux_clauses_0(Compile).

prolog:error_message(connect_shape_mismatch(Plug,Socket,PlugOnly,SocketOnly)) -->
    ["Shape mismatch: cannot connect plug ~w to socket ~w\n"-[Plug,Socket]],
    ["Pins only in plug ~w:~n    ~w\n"-[Plug,PlugOnly]],
    ["Pins only in socket ~w:~n    ~w"-[Socket,SocketOnly]].

%% plug_socket_mapping_clause(+Plug,+Socket,+Mapping,-Clause) is det.

plug_socket_mapping_clause(Plug, Socket, Mapping, Clause) :-
    must_be(ground, Plug),
    must_be(ground, Socket),
    must_be(ground, Mapping),
    qcon_module_name(Plug, Source, _),
    qcon_module_name(Socket, Target, _),
    Mapping = SName/SArity - TName/TArity,
    \+ (SArity \= TArity, throw_error(
        arity_mismatch(Plug,Socket,SName/SArity,TName/TArity)
    )),
    %get_connector_pin(plug, Plug, SName/SArity),
    %get_connector_pin(socket, Socket, TName/TArity),
    Clause = (Target:THead :- Source:SHead),
    length(Args, SArity),
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
    ;   throw_error(too_late_to_run(compile_aux_clauses_0/1)).

prolog:error_message(too_late_to_run(Pred)) -->
    ["It is too late to run ~w.~n"-[Pred]],
    ["It can only run at compile-time, not at run-time.~n"],
    ["Hint: Directives run at compile-time."].

throw_error(E) :- throw(error(E,_)).
