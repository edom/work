:- use_module(library(ansi_term)). % Colorize outputs written from directives.

% ----------------- monkey-patching

/*
Monkey-patching SWI-Prolog 7.6.4 library(error)
because with_output_to/2 clobbers the backtrace.

Who is to blame: with_output_to/2 or setup_call_cleanup/3?

Another problem: If a directive throws an exception, no backtrace is printed.
Only an unhelpful "Goal (directive) failed".
*/

% Turn this to true if you suspect that the backtrace is incomplete.
is_debugging(false).

% -------------------- checking

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

:- use_module(library(error)).

:- abolish(error:domain_error/2).

error:domain_error(T,V) :- fatal(error(domain_error(T,V),_)).

% -------------------- workaround for lost exceptions thrown from directives

% This is for getting the backtrace of exceptions thrown from directives.

:- if(getenv('DEBUG',_)).

    :- debug.

    user:prolog_exception_hook(error(E,_), error(E,Backtrace), _, _) :-
        get_prolog_backtrace(32, Backtrace).

    prolog:message(error(E,C)) -->
        {print_prolog_backtrace(string(S), C)},
        (prolog:error_message(E) ; ["~w"-[E]]), !,
        ["~n~w~n"-[S]].

:- endif.