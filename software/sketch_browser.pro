:- use_module(library(pce),[
    new/2
    , send/2
    , get/3
    , free/1
]).
:- use_module(library(pce_util),[
    send_list/3
]).
:- use_module(library(http/http_open),[
    http_open/3
]).

browser(new) :-
    new(Frame, frame('a web browser?')),
    new(Nav, dialog),
    TestUrl = 'https://www.example.com/',
    send_list(Nav, append, [
        button('quit\n(C-q)', message(Frame,free))
        , button('stop\n(Esc)')
        , button('refresh\n(F5)')
        , new(Address, text_item(address,TestUrl))
        , button('go\n(Enter)', message(@prolog,test,Address?selection))
    ]),
    send_list(Frame, append, [
        Nav
    ]),
    send(Frame, open).

browser(test) :-
    Opts = [
        authenticate(false)
        , connection(close)
        , method(get)
        , redirect(false)
        , status_code(Code)
        , timeout(5)
        , max_redirect(8)
        % https://techblog.willshouse.com/2012/01/03/most-common-user-agents/
        , user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/72.0.3626.121 Safari/537.36')
    ],
    http_open("https://www.example.com/", Stream, Opts),
    write(Code), nl,
    call_cleanup(process(Stream), close(Stream)).

test(Msg) :-
    write(Msg), nl.

% This hangs if connection('Keep-alive').
% Must read Content-Length or detect zero-length chunk.
process(Stream) :-
    current_output(Out),
    copy_stream_data(Stream, Out).
