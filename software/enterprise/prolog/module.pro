/*
    % ---------- declarative
    % -------------------- inputs
    module_definition/2,
    schema_definition/2,
    module_conforms_to_schema/2,
    % -------------------- derived facts
    module/1,
    module_file/2,
    module_import/2,
    module_export/2,
    % -------------------- checks
    check_plugs/0,
    check_connections/0,
    % ---------- deprecated
    module_host/2,
    module_guest/2,
    module_instantiates_ontology/2,
    ontology/1,
    ontology_file/2,
    ontology_module/2,
    ontology_predicate/2,
    % ---------- imperative
    % actions
    load_ontologies/0,
    instantiate_ontologies/0,
    load_modules/0,
    link_modules/0,
    generate_link/3
*/

/** <module> Tailoring SWI-Prolog module system to our requirements

See module.md.
*/

% -------------------- inputs

/** module_definition(?ModuleId,?ModuleDef) is nondet.
    schema_definition(?SchemaId,?SchemaDef) is nondet.
    module_conforms_to_schema(?ModuleId,?SchemaId) is nondet.
    connection(?Plug,?Socket) is nondet.
*/

:- multifile
    module_definition/2,
    schema_definition/2,
    module_conforms_to_schema/2,
    connection/2.

% -------------------- debug

:- debug(module). % DEBUG
%:- debug(module_connection). % DEBUG

debug_module(Format, Args) :-
    string_concat("module: ", Format, Format0),
    debug(module, Format0, Args).

debug_module_connection(Format, Args) :-
    string_concat("module_connection: ", Format, Format0),
    debug(module_connection, Format0, Args).

% -------------------- derived facts

/** module(?ModuleId) is nondet.
    module_file(?ModuleId,?File) is nondet.
    module_import(?ModuleId,?Import) is nondet.
    module_export(?ModuleId,?Export) is nondet.

ModuleId is an atom.
It must be unique among all modules.

Import is an atom that identifies another module.

Export is a Name/Arity term.
*/
:- multifile
    module/1,
    module_file/2,
    module_import/2,
    module_export/2.

module(Id) :- module_definition(Id, _).

module_property(Id, Key, Val) :-
    module_definition(Id, List),
    member(Key-Val, List).

module_name(Id, Name) :- module_property(Id, name, Name), !.
module_name(Id, Name) :- !, Id = Name.
module_file(Id, File) :- module_property(Id, file, File).
module_imports(Id, Imports) :- module_property(Id, imports, Imports).
module_exports(Id, Exports) :- module_property(Id, exports, Exports).

module_file_absolute(Id, File, Absolute) :-
    module_file(Id, File),
    absolute_file_name(File, Absolute).

module_import(Id, Import) :- module_imports(Id, Imports), member(Import, Imports).
module_export(Id, Export) :- module_exports(Id, Exports), member(Export, Exports).

% -------------------- schemas

:- multifile
    schema_plug_pins/3,
    schema_socket_pins/3.

schema_property(Id, Key, Val) :-
    schema_definition(Id, List),
    member(Key-Val, List).

schema_plug_pins(Id, Plug, Pins) :-
    schema_property(Id, plugs, Plugs),
    member(Plug-Pins, Plugs).

schema_socket_pins(Id, Plug, Pins) :-
    schema_property(Id, sockets, Plugs),
    member(Plug-Pins, Plugs).

schema_plug_pin(Id, Plug, Pin) :-
    schema_plug_pins(Id, Plug, Pins),
    member(Pin, Pins).

schema_socket_pin(Id, Socket, Pin) :-
    schema_socket_pins(Id, Socket, Pins),
    member(Pin, Pins).

% -------------------- schemas and modules

module_schema_plug_pin(Module, Schema, Plug, Pin) :-
    module_conforms_to_schema(Module, Schema),
    schema_plug_pin(Schema, Plug, Pin).

module_schema_socket_pin(Module, Schema, Socket, Pin) :-
    module_conforms_to_schema(Module, Schema),
    schema_socket_pin(Schema, Socket, Pin).

:- include('module_check.pro').
:- include('module_deprecated.pro').
:- include('module_load.pro').
