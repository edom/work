:- module(prolog_component,[
    connect_plug_to_socket/2
]).
/** <module> Socket-plug metaphor

A pin is a Name/Arity term.

A plug exports symbols.

A socket imports symbols.

A module may have multiple plugs and sockets.

Pins are matched by NameArity.
The ordering of pins does not matter.
*/

/** '$socket'(?SocketName,?SocketPins) is nondet.
    '$plug'(?PlugName,?PlugPins) is nondet.

Usage:
    - Define `$plug` in the module that exports the symbol.
    - Define `$socket` in the module that imports the symbol.
*/
module_plug(Module,Plug,Pins) :- Module:'$plug'(Plug,Pins).
module_socket(Module,Socket,Pins) :- Module:'$socket'(Socket,Pins).

/** connect_plug_to_socket(?SourcePlug,?TargetSocket)

*/
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
