:- module(main,[
    main/1
    , run/0
]).

:- use_module('./syntax.pro',[]).
:- use_module('./main_library.pro',[
    setup_translation/3
]).
:- use_module('./language/pal_interpreter.pro',[]).

/** <module> Enterprise model?

*/



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
    translate(accounting).

translate(ModulePrefix) :-
    ModulePrefix:check,
    ModulePrefix:generate.



% -------------------- translation pipelines



:-  DryRun = false,
    setup_translation(accounting, 'spec/accounting.pro', [
        base_package_name-"com.spacetimecat.java"
        , maven_coordinates-[
            group_id-"com.spacetimecat.java"
            , artifact_id-"accounting"
            , version-"0.0.0"
        ]
        , output_dir-"out"
        , dry_run-DryRun
    ]).



:- if(current_prolog_flag(argv,[])).
:- else.
    :- initialization(main,main).
:- endif.
