% -------------------- checks

/** check_plugs is semidet.

Print each module that does not export the pins it claims to export.
*/

check_plugs :- \+ undefined_plug(_,_,_,_), !.
check_plugs :-
    undefined_plug(Module, Schema, Plug, Pin),
    deterministically((
        module_file(Module, File),
        print_message(error, plug_not_defined(Module,File,Schema,Plug,Pin))
    )),
    fail.

undefined_plug(Module, Schema, Plug, Pin) :-
    module(Module),
    module_schema_plug_pin(Module, Schema, Plug, Pin),
    module_name(Module, ModName),
    pin_functor(Pin, Functor),
    \+ predicate_property(ModName:Functor, defined).

pin_functor(Name/Arity, Head) :-
    functor(Head, Name, Arity).

/** check_connections is semidet.
*/

check_connections :-
    check_unconnected,
    check_schema_connections.

check_unconnected :- \+ unconnected(_, _, _, _), !.
check_unconnected :-
    unconnected(Module, Schema, Socket, Pin),
    print_message(error, socket_not_connected(Module,Schema,Socket,Pin)),
    fail.

unconnected(Module, Schema, Socket, Pin) :-
    module(Module),
    module_schema_socket_pin(Module, Schema, Socket, Pin),
    \+ connection(_, [Module,Socket,Pin]).

check_schema_connections :- \+ schema_socket_plugs_invalid(_, _, _), !.
check_schema_connections :-
    schema_socket_plugs_invalid(Schema, Socket, Plugs),
    print_message(error, socket_not_connected_exactly_once(Schema:Socket,Plugs)),
    fail.

schema_socket_plugs_invalid(Schema2, Socket, Sources) :-
    schema_definition(Schema2, _),
    schema_socket_pins(Schema2, Socket, _),
    findall(Schema1:Plug,
        (
            schema_definition(Schema1, _),
            schema_plug_pins(Schema1, Plug, _),
            schema_connection(Schema1:Plug, Schema2:Socket)
        ),
        Sources),
    Sources \= [_].

prolog:message(plug_not_defined(Module,File,Schema,Plug,Pin)) -->
    ["Plug pin is not defined:~n"],
    ["~n"],
    ["    Module : ~w~n"-[Module]],
    ["    File   : ~w~n"-[File]],
    ["    Schema : ~w~n"-[Schema]],
    ["    Plug   : ~w~n"-[Plug]],
    ["    Pin    : ~w~n"-[Pin]].

prolog:message(socket_not_connected(Module,Schema,Socket,Pin)) -->
    ["Socket pin is not connected:~n"],
    ["~n"],
    ["    Module : ~w~n"-[Module]],
    ["    Schema : ~w~n"-[Schema]],
    ["    Socket : ~w~n"-[Socket]],
    ["    Pin    : ~w~n"-[Pin]].

prolog:message(socket_not_connected_exactly_once(Socket,Plugs)) -->
    ["Socket is not connected exactly once:~n"],
    ["~n"],
    ["    Socket : ~w~n"-[Socket]],
    ["    Plugs  : ~w~n"-[Plugs]].
