% -------------------- loading

/** load_modules is det.

Load the modules defined by module/2.

Generate links according to module_guest/2.

Known problem: No dependency analysis.
The order of clauses of module/1 is the order in which the modules are loaded.
If A imports B, then you must make sure that module/1 produces B before A.
Clause ordering matters.
*/

load_modules :-
    forall(module(Module), load_module(Module)),
    debug_module("Connecting plugs to sockets", []),
    forall(
        connection(A,B),
        deterministically((
            A = [Source,Plug,PlugPin],
            B = [Target,Socket,SocketPin],
            module_name(Source, SourceName),
            module_name(Target, TargetName),
            debug_module_connection("plug:socket ~w:~w  ~w:~w  ~w:~w",
                [SourceName,TargetName,Plug,Socket,PlugPin,SocketPin]),
            plug_socket_mapping_clause(SourceName:Plug, TargetName:Socket, PlugPin-SocketPin, Clause),
            compile_aux_clauses([Clause])
        ))
    ).

make_conform(Module, Schema) :- deterministically((
    ground(Module),
    debug_module("Making module ~w conform to schema ~w", [Module,Schema]),
    module_name(Module, Name),
    forall(
        schema_socket_pin(Schema, _, Pin),
        multifile(Name:Pin)
    )
)).

load_module(Module) :- deterministically((
    ground(Module),
    module_name(Module, Name),
    module_file(Module, File),
    debug_module("Loading module ~w as ~w from ~w", [Module,Name,File]),
    forall(module_import(Module,Import), do_module_import(Module,Import)),
    Name:load_files(File, [module(Name)]),
    forall(module_export(Module,Export), Name:export(Export)),
    forall(
        module_conforms_to_schema(Module,Schema),
        make_conform(Module,Schema)
    )
)).

do_module_import(Module, module(Import)) :- !, deterministically((
    module_name(Import, Name),
    do_module_import(Module, module_name(Name))
)).

do_module_import(Module, module_name(Import)) :- !, deterministically((
    module_name(Module,Target),
    add_import_module(Target, Import, end)
)).

do_module_import(_, Import) :-
    domain_error(module_import, Import).

/*
==================== moved from prolog_component.pro

These are probably dead codes.
*/

:- use_module(library(debug)).

% -------------------- tracing

debug_connector(Format, Args) :-
    context_module(Module),
    format(string(Format0), "~w: ~w", [Module,Format]),
    debug(connector, Format0, Args).

% -------------------- deconstruction

qcon_module_name(Module:Name, A, B) :- !, A = Module, B = Name.
qcon_module_name(Con, _, _) :- type_error(qualified_connector_name,Con).

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
    Connect = connected(Plug,Socket),
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