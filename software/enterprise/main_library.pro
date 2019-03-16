:- module(main_library,[

    % -------------------- checking

    fatal/1

    % -------------------- wiring the translation pipeline

    , setup_translation/3
]).

/** <module> Common things encountered when writing a main module

Functionalities that help drive the translation process.
*/

:- use_module('./prolog/customization.pro',[
    load_module_from_file/2
    , do_module_import/2
    , compile_aux_clauses_0/1
    , declare_plug/2
    , declare_socket/2
    , connect_plug_to_socket/2
    , connect_plug_to_socket/3
]).



/* ------- monkey-patching
Monkey-patching SWI-Prolog 7.6.4 library(error)
because with_output_to/2 clobbers the backtrace.

Who is to blame: with_output_to/2 or setup_call_cleanup/3?

Another problem: If a directive throws an exception, no backtrace is printed.
Only an unhelpful "Goal (directive) failed".
*/

% Turn this to true if you suspect that the backtrace is incomplete.
is_debugging(false).



/** fatal(?Error) is failure.

Prints back-trace where it is raised, if not is_debugging(false).
*/

fatal(E) :- is_debugging(false), !, throw(E).
fatal(E) :- print_message(error,E),
    get_prolog_backtrace(32,Backtrace),
    % get_prolog_backtrace, get_prolog_backtrace, fatal
    Backtrace = [_,_,_|Backtrace0],
    print_prolog_backtrace(user_error,Backtrace0),
    throw(E).



:- abolish(error:domain_error/2).

error:domain_error(T,V) :- fatal(error(domain_error(T,V),_)).



% ------- subsumption



subsume(L,R) :-
    foreach(module_host(L,Pred), subsume(L,R,Pred)).

subsume(L,R,Name/Arity) :-
    functor(Head,Name,Arity),
    (predicate_property(R:Head,defined)
    ->  assertz(L:Head :- R:Head)
    ;   print_message(warning, subsume_undefined_predicate(L,R,Name/Arity))
    ).

prolog:message(subsume_undefined_predicate(L,R,P)) -->
    ["Subsumption ~w :- ~w failed for predicate ~w"-[L,R,P]].

do_module_load_model(Module, File) :-
    print_message(informational,loading_module(Module, File)),
    Module:use_module('./syntax.pro'),
    Module:use_module('./prolog/customization.pro'),
    Module:load_files(File, [module(Module)]).

do_module_instantiate_schema(Module, File) :-
    Module:use_module('./prolog/customization.pro'),
    load_module_from_file(Module, File).



% -------------------- wiring the translation pipeline



/** setup_translation(+ModulePrefix,+ModelFile,+Params) is det.

An example predefined translation pipeline from one system to one Java web application.

Params is a list of translation parameters containing these elements:

    - base_package_name-BasePackageName
    Base Java package name.

    - maven_coordinates-MavenCoords
    MavenCoords is a list.

        - group_id-GroupId
        Maven group id.

        - artifact_id-ArtifactId

        - version-Version

How do we make this more flexible?
*/

setup_translation(ModulePrefix, ModelFile, Params) :-

    % Get translation parameters.

    get_dictionary_key_value_1(Params, base_package_name, BasePackageName),
    get_dictionary_key_value_1(Params, maven_coordinates, MavenCoords),
    get_dictionary_key_value_1(Params, dry_run, DryRun),
    get_dictionary_key_value_1(MavenCoords, group_id, GroupId),
    get_dictionary_key_value_1(MavenCoords, artifact_id, ArtifactId),
    get_dictionary_key_value_1(MavenCoords, version, Version),

    % Allocate module names.
    % I_ = a world, a schema instance, a module that conforms to a schema
    % T_ = a translation, a transformation

    atom_concat(ModulePrefix, '_model', Model),
    atom_concat(ModulePrefix, '_system', I_system),
    atom_concat(ModulePrefix, '_web_app', I_web_app),
    atom_concat(ModulePrefix, '_java_program', I_program),
    atom_concat(ModulePrefix, '_webapp_javaprogram', T_webapp_javaprogram),
    atom_concat(ModulePrefix, '_java_writer', T_writer),

    % Load modules.

    do_module_load_model(Model, ModelFile),
    do_module_instantiate_schema(I_system, 'schema/system.pro'),
    do_module_instantiate_schema(I_web_app, 'schema/web_application.pro'),
    do_module_instantiate_schema(I_program, 'schema/java_program.pro'),
    load_module_from_file(T_webapp_javaprogram, 'translation/webapp_javaprogram.pro'),
    load_module_from_file(T_writer, './java_writer.pro'),

    % Link modules.

    declare_plug(Model:type_convenient,[
        type_definition/2
    ]),
    declare_plug(Model:type_refinement,[
        type_maxbitcount/2
        , type_maxbytecount/2
    ]),
    declare_plug(Model:globalization,[
        term_locale_string/3
    ]),
    declare_plug(Model:system,[
        state/1
        , state_type/2
        , state_initializer/2
        , procedure/1
        , procedure_name/2
        , procedure_input/3
        , procedure_check/2
        , procedure_output/2
    ]),
    forall(
        member(Port,[type_convenient,type_refinement,system,globalization]),
        connect_plug_to_socket(Model:Port, I_system:Port)
    ),
    subsume(I_web_app, Model),
    subsume(T_webapp_javaprogram, Model),
    subsume(I_program, T_webapp_javaprogram),
    do_module_import(T_webapp_javaprogram,[
        I_system:type_integer_bit/2
        , I_system:type_identifier_bit/2
        , I_system:type_string/1
        , I_system:type_optional/2
        , I_system:recordtype/1
    ]),
    (
        compile_aux_clauses_0([
            T_webapp_javaprogram:base_package_name(BasePackageName)
            , I_program:maven_coordinates(GroupId,ArtifactId,Version)
            , T_writer:output_dir("out")
            , T_writer:dry_run(DryRun)
            , ModulePrefix:check :- I_program:check_ontology
            , ModulePrefix:generate :- T_writer:generate
        ])
        ->  true
        ;   throw(error(too_late_to_run(setup_translation/3),_))
    ),
    add_import_module(T_writer, I_program, end),
    true.

get_dictionary_key_value_1(_, Key, _) :- var(Key), !, instantiation_error(Key).
get_dictionary_key_value_1(List, Key, Value) :- member(Key-Value, List), !.
get_dictionary_key_value_1(List, Key, _) :- throw(error(no_dictionary_key(List,Key),_)).

prolog:error_message(no_dictionary_key(List,Key)) -->
    ["Key ~w is not in dictionary ~w."-[Key,List]].
