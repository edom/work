:- module(main_library,[
    load_spec/2
    , subsume/2
    , subsume/3
    % checking
    , fatal/1
    % socket-plug metaphor
    , connect_plug_to_socket/2
]).
/** <module> Common things encountered when writing a main module

Functionalities that help drive the translation process.
*/
:- use_module(library(error)).
:- use_module('./modules.pro',[]).

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
    foreach(modules:module_host(L,Pred), subsume(L,R,Pred)).

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
    %foreach(modules:module_host(ontology_system,Pred), Module:discontiguous(Pred)),
    %foreach(modules:module_host(ontology_web_application,Pred), Module:discontiguous(Pred)),
    %foreach(modules:module_host(ontology_relational_databases,Pred), Module:discontiguous(Pred)),
    Module:consult(File).

% ------- socket-plug metaphor

/** '$socket'(?SocketName,?SocketPins) is nondet.
    '$plug'(?PlugName,?PlugPins) is nondet.

A pin is a Name/Arity term.

A plug exports symbols.

A socket imports symbols.

A module may have multiple plugs and sockets.

Pins are matched by NameArity.
The ordering of pins does not matter.

Usage:
    - Define `$plug` in the module that exports the symbol.
    - Define `$socket` in the module that imports the symbol.
*/
module_plug(Module,Plug,Pins) :- Module:'$plug'(Plug,Pins).
module_socket(Module,Socket,Pins) :- Module:'$socket'(Socket,Pins).

connect_plug_to_socket(Source:Plug,Target:Socket) :-
    module_plug(Source,Plug,PlugPins),
    module_socket(Target,Socket,SocketPins),
    subtract(PlugPins,SocketPins,PlugOnly),
    subtract(SocketPins,PlugPins,SocketOnly),
    ((PlugOnly == [], SocketOnly == [])
        ->  true
        ;   throw(error(connect_shape_mismatch(Source:Plug,Target:Socket,PlugOnly,SocketOnly)))),
    foreach(member(Pin,PlugPins), connect_1(Source,Target,Pin)).

    connect_1(Source,Target,Name/Arity) :-
        functor(Head,Name,Arity),
        assertz(Target:Head :- Source:Head).

    prolog:error_message(connect_shape_mismatch(SourcePlug,TargetSocket,PlugOnly,SocketOnly)) -->
        ["Shape mismatch: cannot connect plug ~w to socket ~w\n"-[SourcePlug,TargetSocket]],
        ["Pins only in plug: ~w\n"-PlugOnly],
        ["Pins only in socket: ~w"-SocketOnly].
