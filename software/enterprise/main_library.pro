:- module(main_library,[
    load_spec/2
    , subsume/2
    , subsume/3
    % checking
    , fatal/1
]).
/** <module> Common things encountered when writing a main module

Functionalities that help drive the translation process.
*/
:- use_module(library(error)).
:- use_module('./prolog/customization.pro',[
    module_host/2
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

load_spec(Module,File) :-
    print_message(informational,loading_module(Module,File)),
    Module:use_module('./syntax.pro'),
    %foreach(module_host(schema_system,Pred), Module:discontiguous(Pred)),
    %foreach(module_host(schema_web_application,Pred), Module:discontiguous(Pred)),
    %foreach(module_host(schema_relational_databases,Pred), Module:discontiguous(Pred)),
    Module:consult(File).
