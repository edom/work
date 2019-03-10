:- module(main,[
    main/1
    , run/0
]).
/** <module> Enterprise model/ontology?

*/

:- use_module('./syntax.pro',[]).
:- use_module('./modules.pro',[]).
:- use_module('./main_library.pro',[
    load_spec/2
    , subsume/2
    , subsume/3
]).

% ------- instantiate module

/** load_module_from_file(+Module,+File) is det.

The same file can be loaded into different modules.
*/
load_module_from_file(Module,File) :-
    Module:load_files(File, [module(Module),register(false)]).

% ------- wiring

:- load_spec(accounting_spec,'spec/accounting.pro').
:- load_module_from_file(accounting_system,'./ontology_system.pro').
:- subsume(accounting_system,accounting_spec).

:- load_module_from_file(accounting_web_app,'./ontology_web_application.pro').
:- subsume(accounting_web_app,accounting_spec).

:- load_module_from_file(accounting_translation,'./java_program_from_web_app.pro').
:- subsume(accounting_translation,accounting_spec).
:- accounting_translation:import(accounting_system:type_integer_bit/2).
:- accounting_translation:import(accounting_system:type_identifier_bit/2).
:- accounting_translation:import(accounting_system:type_string/1).
:- accounting_translation:import(accounting_system:type_optional/2).
:- accounting_translation:import(accounting_system:recordtype/1).
accounting_translation:recordtype_field(T,F,FN,FT) :-
    accounting_system:recordtype_field(T,F),
    accounting_system:field_name(F,FN),
    accounting_system:field_type(F,FT).
accounting_translation:default_package_name("com.spacetimecat.java").

:- load_module_from_file(accounting_java_program,'./ontology_java_program.pro').
:- subsume(accounting_java_program,accounting_translation).
accounting_java_program:maven_coordinates('com.spacetimecat.java',accounting,'0.0.0').

:- load_module_from_file(accounting_java_writer,'./java_writer.pro').
:- add_import_module(accounting_java_writer,accounting_java_program,end).
accounting_java_writer:output_dir("out").
accounting_java_writer:dry_run(false).

%:- load_spec(employee_spec,'spec/employee.pro').

/** main(++Args)

Entry point.
*/
main([run]) :- !, run.
main(Args) :- throw(error(unknown_args(Args),_)).

prolog:error_message(unknown_args(Args)) -->
    ['unknown command-line arguments ~w'-[Args]].

/** run

This is called by main/1,
but can also be run directly from the interpreter prompt for testing.
*/
run :-
    accounting_java_program:check_ontology,
    accounting_java_writer:generate.

:- if(current_prolog_flag(argv,[])).
:- else.
    :- initialization(main,main).
:- endif.
