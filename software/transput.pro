:- module(transput, [
    with_file/5
    , file_begin_count_bytes/4
    % Seekable stream.
    , stream_begin_count_bytes/4
]).

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
