:- module(database_example, [
    test/0
]).
:- use_module('./database.pro').
test :-
    Type = functor(animal, [u32, vchars1(27), vchars1(31)]),
    assertion(type_bytecount(Type,32)),
    setup_call_cleanup(
        db_open_file('example.db', Type, Handle),
        use(Handle),
        db_close(Handle)
    ).

use(Handle) :-
    db_write(Handle, [animal(1, "dog", "woof"), animal(2, "cat", "meow")]),
    db_seek(Handle, 0),
    length(Elems, 2),
    db_read(Handle, Elems),
    print(Elems), nl.
