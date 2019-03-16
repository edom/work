deterministically((A,B)) :- !,
    deterministically(A),
    deterministically(B).

deterministically(A) :- A -> true ; throw_error(should_not_fail(A)).

throw_error(E) :- throw(error(E,_)).

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
            prolog_component:plug_socket_mapping_clause(SourceName:Plug, TargetName:Socket, PlugPin-SocketPin, Clause),
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

link_modules :-
    false.%TODO
