% -------------------- inputs

/** module_definition(?ModuleId,?ModuleDef) is nondet.

ModuleDef is a list of key-value pairs:

    - file-File
    File contains the source code for the module.

    - imports-Imports
    Imports is a list of imports:

        - module(ModuleId)
        Import a module defined by module_definition/2.
        Note that ModuleId is a primary key to module_definition/2:
        ModuleId is not the name of a loaded Prolog module.

    - exports-Exports
    Imports is a list of Name/Arity terms.
*/

/** schema_definition(?SchemaId,?SchemaDef) is nondet.
    module_conforms_to_schema(?ModuleId,?SchemaId) is nondet.
    connection(?Plug,?Socket) is nondet.
*/

:- multifile
    module_definition/2,
    schema_definition/2,
    module_conforms_to_schema/2,
    connection/2.

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
