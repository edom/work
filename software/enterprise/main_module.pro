% -------------------- infrastructure modules

module_definition(prolog_customization,[
    file-"prolog/customization.pro"
    , description-"Tailoring SWI-Prolog 7.6.4"
    , exports-[
        load_module_from_file/2
        , do_module_import/2
        , throw_error/1
    ]
]).

module_definition(syntax,[
    file-"syntax.pro"
    , exports-[
        '#'/1
    ]
]).

%-------------------- pipeline modules

module_definition(model(Model,model),[
    name-ModuleName
    , file-File
    , imports-[
        module(syntax)
        , module(prolog_customization)
    ]
]) :-
    model(Model),
    model_name(Model, ModelName),
    model_file(Model, File),
    atom_concat(ModelName, '_model', ModuleName).

module_definition(model(Model,system),[
    name-ModuleName
    , file-"schema/system.pro"
    , imports-[module(syntax)]
]) :-
    model(Model),
    model_name(Model, ModelName),
    atom_concat(ModelName, '_system', ModuleName).

module_definition(model(Model,webapp),[
    name-ModuleName
    , file-"schema/web_application.pro"
    , imports-[module(syntax)]
]) :-
    model(Model),
    model_name(Model, ModelName),
    atom_concat(ModelName, '_webapp', ModuleName).

module_definition(webapp_javaprogram(Model),[
    name-ModuleName
    , file-"translation/webapp_javaprogram.pro"
    , imports-[module(syntax)]
]) :-
    model(Model),
    model_name(Model, ModelName),
    atom_concat(ModelName, '_webapp_javaprog', ModuleName).

module_definition(model(Model,java_program),[
    name-ModuleName
    , file-"schema/java_program.pro"
    , imports-[module(syntax)]
]) :-
    model(Model),
    model_name(Model, ModelName),
    atom_concat(ModelName, '_javaprog', ModuleName).

module_definition(java_writer(Model),[
    name-ModuleName
    , file-"java_writer.pro"
    , imports-[module(syntax)]
]) :-
    model(Model),
    model_name(Model, ModelName),
    atom_concat(ModelName, '_javawriter', ModuleName).

% -------------------- models

model_definition(accounting,[
    file-"spec/accounting.pro"
]).

model(Id) :- model_definition(Id, _).

model_name(Id, Id).

model_property(Id, Key, Value) :-
    model_definition(Id, List),
    member(Key-Value, List).

model_file(Id, File) :- model_property(Id, file, File).

% -------------------- modules and schemas

module_conforms_to_schema(model(_,Schema), Schema).
module_conforms_to_schema(webapp_javaprogram(_), webapp_javaprogram).
module_conforms_to_schema(java_writer(_), java_writer).

% -------------------- connections

connection([ModSrc,Plug,Pin], [ModTar,Socket,Pin]) :-
    module(ModSrc),
    module(ModTar),
    module_conforms_to_schema(ModSrc, SchSrc),
    module_conforms_to_schema(ModTar, SchTar),
    schema_connection(SchSrc:Plug, SchTar:Socket),
    schema_plug_pin(SchSrc, Plug, Pin),
    schema_socket_pin(SchTar, Socket, Pin).

connection([System,Con,Pin], [Webapp,Con,Pin]) :-
    System = model(M,system),
    Webapp = model(M,webapp),
    module(System),
    module(Webapp),
    module_conforms_to_schema(System, system),
    module_conforms_to_schema(Webapp, webapp),
    schema_plug_pin(system, Con, Pin),
    schema_socket_pin(webapp, Con, Pin).

%-------------------- checks

check :-
    debug_module("Checking plugs", []),
    check_plugs,
    debug_module("Checking connections", []),
    check_connections,
    forall(model(Model), check(Model)).

check(Model) :-
    module_name(model(Model,java_program), Module),
    Module:check_ontology.

generate :-
    forall(model(Model), generate(Model)).

generate(Model) :-
    module_name(java_writer(Model), Module),
    Module:generate.
