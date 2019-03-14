:- module(main_library,[

    % -------------------- checking

    fatal/1

    % -------------------- wiring the translation pipeline

    , setup_translation/3
]).

/** <module> Common things encountered when writing a main module

Functionalities that help drive the translation process.
*/

:- use_module(library(error)).
:- use_module('./prolog/customization.pro',[
    module_host/2
    , load_module_from_file/2
    , do_module_import/2
]).



/* ------- monkey-patching
Monkey-patching SWI-Prolog 7.6.4 library(error)
because with_output_to/2 clobbers the backtrace.

Who is to blame: with_output_to/2 or setup_call_cleanup/3?
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
    Module:load_files(File, [module(Module)]).

do_module_instantiate_schema(Module, File) :-
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
    atom_concat(ModulePrefix, '_translation', T_java_web),
    atom_concat(ModulePrefix, '_java_writer', T_writer),

    % Load modules.

    do_module_load_model(Model, ModelFile),
    do_module_instantiate_schema(I_system, './schema/system.pro'),
    do_module_instantiate_schema(I_web_app, './schema/web_application.pro'),
    do_module_instantiate_schema(I_program, './schema/java_program.pro'),
    load_module_from_file(T_java_web, './java_program_from_web_app.pro'),
    load_module_from_file(T_writer, './java_writer.pro'),

    % Link modules.

    subsume(I_system, Model),
    subsume(I_web_app, Model),
    subsume(T_java_web, Model),
    subsume(I_program, T_java_web),
    do_module_import(T_java_web,[
        I_system:type_integer_bit/2
        , I_system:type_identifier_bit/2
        , I_system:type_string/1
        , I_system:type_optional/2
        , I_system:recordtype/1
    ]),
    (
        compile_aux_clauses_0([
            (T_java_web:recordtype_field(T,F,FN,FT) :-
                I_system:recordtype_field(T,F),
                I_system:field_name(F,FN),
                I_system:field_type(F,FT)
            )
            , T_java_web:default_package_name(BasePackageName)
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

get_dictionary_key_value_1(List, Key, Value) :- var(Key), !, instantiation_error(Key).
get_dictionary_key_value_1(List, Key, Value) :- member(Key-Value, List), !.
get_dictionary_key_value_1(List, Key, Value) :- throw(error(no_dictionary_key(List,Key),_)).

prolog:error_message(no_dictionary_key(List,Key)) -->
    ["Key ~w is not in dictionary ~w."-[Key,List]].
