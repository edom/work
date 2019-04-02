% -------------------- connection management

% Restart your program often if you use this.

% Problem: Debugging prints the password.

:- debug(sql_connect). % DEBUG

:- use_module(library(odbc),[
    odbc_driver_connect/3
]).

:- dynamic connection/3. % Name, OdbcCon, CreationTime

% Problem: Race condition when several threads call ensure_opened.

%%  ensure_opened(++Name, ++Dsn, --Con) is det.

ensure_opened(Name, Dsn, Con) :-
    must_be(atom, Name),
    must_be(string, Dsn),
    must_be(var, Con),
    connection(Name, Con, _), !.

ensure_opened(Name, Dsn, Con) :- !,
    debug(sql_connect, "sql_connect: Opening ~w as ~w", [Name, Dsn]),
    odbc_driver_connect(Dsn, Con, []),
    get_time(Time),
    assertz(connection(Name,Con,Time)).

%%  ensure_closed(++Name) is det.

ensure_closed(Name) :-
    must_be(atom, Name),
    debug(sql_connect, "sql_connect: Closing ~w", [Name]),
    retract(connection(Name,Con,_)), !,
    odbc_disconnect(Con).

ensure_closed(_) :- !.

close_all_connections :-
    forall(connection(Name,_,_), ensure_closed(Name)).
