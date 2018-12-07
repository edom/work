:- module(transput, [
    with_file/5
    , file_begin_count_bytes/4
    % Seekable stream.
    , stream_begin_count_bytes/4
    , get_bytes/2
    , put_bytes/2
]).
/** <module> input-output

- get_bytes/2 and put_bytes/2: repeated get_byte/2 and put_byte/2
*/

:- meta_predicate
    with_file(?, ?, ?, ?, 0)
    .

with_file(Path, Mode, Opts, Stream, Goal) :-
    setup_call_cleanup(
        open(Path, Mode, Stream, Opts),
        Goal,
        close(Stream)
    ).

file_begin_count_bytes(Path, Begin, Count, Bytes) :-
    with_file(
        Path, read, [type(binary)], Stream,
        stream_begin_count_bytes(Stream, Begin, Count, Bytes)
    ).

stream_begin_count_bytes(Stream, Begin, Count, Bytes) :-
    seek(Stream, Begin, bof, _),
    peek_string(Stream, Count, String),
    string_codes(String, Bytes).

/** get_bytes(++Stream, +Bytes)

This is repeated get_byte/2.

The length of Bytes should be bound.

If Bytes is unbound, this read the remaining bytes in the stream.
*/
get_bytes(Stream, [H|T]) :- get_byte(Stream, H), H >= 0, !, get_bytes(Stream, T).
get_bytes(_, []).

/** put_bytes(++Stream, ++Bytes)

This is repeated put_byte/2.

Bytes is a list of bytes (integers between 0 and 255 inclusive).
*/
put_bytes(Stream, [H|T]) :- !, put_byte(Stream, H), put_bytes(Stream, T).
put_bytes(_, []).
