cl_if(C,G) :- C, !, G.
cl_if(_,_).

cl_if_not(C,_) :- C, !.
cl_if_not(_,G) :- G.

/** browse is det.
*/
browse :-
    catch(browse_0, E,
        (
            print_message(error, E),
            cl_if(E \= quit, browse)
        )
    ).

browse_0 :-
    write("\nPress a key (h for help): "),
    get_single_char_0(Code), nl,
    code_command(Code, Command),
    run(Command),
    browse_0.

format_title(Format, Args) :-
    format(string(Title), Format, Args),
    format("\e[1m~`-t ~w ~`-t~80|\e[22m\n", [Title]).

get_single_char_0(Code) :-
    repeat,
    get_single_char(Code),
    Code \= 0'\r,
    Code \= 0'\n,
    !.

code_command(-1, quit) :- !.
code_command(12, clear) :- !.
code_command(0'h, help) :- !.
code_command(0'q, quit) :- !.
code_command(0'l, list) :- !.
code_command(0's, show) :- !.
code_command(Code, _) :- !, throw(unknown_character_code(Code)).

run(quit) :- !, throw(quit).
run(nop) :- !.
run(clear) :- !, tty_clear.
run(list) :- !, list.
run(help) :- !,
    format_title("Help", []),
    write("Commands:

    Ctrl+L  clear screen
    h       show this help
    q       quit documentation browser
    l       list documented things
    s       show documentation for thing
").
run(show) :- !,
    write("Input the thing to show, end with a period:\n"),
    read(Thing),
    show(Thing).
run(Command):- !, throw(unknown_command(Command)).

list :- deterministically((
    setof(Thing, D^documentation(Thing,D), Things),
    length(Things, Count),
    format_title("List of ~w documented things",[Count]),
    forall(member(Thing, Things),
        deterministically((
            thing_summary(Thing, Summary),
            markup_render(Summary, String),
            format("~w~32|~w\n", [Thing,String])
        ))
    )
)).

markup_render(Markup, Render) :-
    phrase(markup(Markup), Codes, []),
    string_codes(Render, Codes).

show(Thing) :- \+documentation(Thing,_), !, print_message(error, undocumented(Thing)).
show(Thing) :- deterministically((
    documentation(Thing, Doc),
    term_string(Thing, SThing),
    (member(summary-Summary, Doc) ; Summary = empty),
    (member(detail-Detail, Doc) ; Detail = empty),
    markup_render(title(SThing), STitle),
    markup_render(Summary, SSummary),
    markup_render(Detail, SDetail),
    format("\n~w\n\n~w\n~w~n", [STitle, SSummary, SDetail])
)).

markup(empty) --> !, [].
markup(title(S)) --> !, "\e[1m", S, "\e[22m". % bold
markup(string(S)) --> !, S.
markup(A) --> {throw(error(invalid_markup(A),_))}.

prolog:message(undocumented(Thing)) -->
    ["No documentation for ~w.\n"-[Thing]].