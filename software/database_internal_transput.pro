:- module(database_internal_transput, [
    db_open_file/3,
    db_close/1,
    db_flush/1,
    db_seek/2,
    db_read/2,
    db_write/2
]).
:- use_module(library(clpfd)).
:- use_module(library(error)).
:- use_module('./database_internal_dcg.pro').
:- use_module('./database_internal_pure.pro').
:- use_module('./transput.pro').
/** <module> database.pro internals; do not use
*/

/**
handle_streams_type(Handle, ReadStream, WriteStream, Type).
handle_stride(Handle, Stride).
handle_elemofs_byteofs(Handle, ElemOffset, ByteOffset).
handle_elemcount_bytecount(Handle, ElemCount, ByteCount).

Internal representation used by low-level transput predicates such as db_open_file/3.

Stride is the number of bytes per element.

ByteOffset is ElemOffset multiplied by Stride.
At least one of ByteOffset or ElemOffset must be ground.
*/
handle_streams_type(Handle, SR, SW, Type) :-
    Handle = handle(SR,SW,Type,Stride),
    type_bytecount(Type,Stride).

handle_streams(Handle, SR, SW) :- arg(1, Handle, SR), arg(2, Handle, SW).
handle_stride(Handle, Stride) :- arg(4, Handle, Stride).
handle_streams_stride(Handle, SR, SW, Stride) :-
    handle_streams(Handle, SR, SW),
    handle_stride(Handle, Stride).
handle_streams_type_stride(Handle, SR, SW, Type, Stride) :-
    handle_streams_type(Handle, SR, SW, Type),
    handle_stride(Handle, Stride).

handle_elemofs_byteofs(H, E, B) :- handle_stride(H, S), B #= E * S.
handle_elemcount_bytecount(H, E, B) :- handle_stride(H, S), B #= E * S.

/** db_open_file(++Path, ++Type, --Handle)

Open a database file.

Type is defined in type_value//2.

Important:
    - If you change Type, make sure that your change is backward-compatible with the data that is already stored in the file.

Non-portability:
    - This assumes that SWI-Prolog will lock the file.
    This uses the SWI-Prolog-specific lock(read) and lock(write) option of open/4.
    - SWI-Prolog os/pl-file.c/openStream calls os/pl-stream.c/Sopen_file
    which never passes O_RDWR to open(2),
    so we have to open two streams: one for reading, one for "updating" (writing without truncating the file).
    - ISO Prolog open/4 does not have a way of opening a file for writing without truncating a file.
    What the hell.

See also:
seek/4, read_string/3.
*/
db_open_file(Path, Type, Handle) :-
    must_be(ground, Path),
    must_be(ground, Type),
    (type_bytecount(Type, _) -> true ; type_error(type, Type)),
    catch_all(
        [
            open(Path, update, SW, [type(binary), lock(write), wait(false)]),
            open(Path, read, SR, [type(binary), lock(read), wait(false)])
        ], Es),
    (Es = [] -> handle_streams_type(Handle, SR, SW, Type)
    ;   (ground(SR) -> close(SR) ; true),
        (ground(SW) -> close(SW) ; true),
        rethrow(Es)
    ).

/**
db_close(++Handle).
db_flush(++Handle).
db_seek(++Handle, ++ElemOffset).
db_write(++Handle, ++Elems).

Low-level transput.
*/

db_close(Handle) :-
    handle_streams(Handle, SR, SW),
    catch_all([close(SR),close(SW)], Es),
    (Es = [] -> true ; rethrow(Es)).

db_flush(Handle) :-
    handle_streams(Handle, SR, SW),
    flush_output(SR),
    flush_output(SW).

db_seek(Handle, ElemOffset) :-
    handle_streams(Handle, SR, SW),
    handle_elemofs_byteofs(Handle, ElemOffset, ByteOffset),
    seek(SR, ByteOffset, bof, _),
    seek(SW, ByteOffset, bof, _).

db_write(Handle, Elems) :-
    must_be(list, Elems),
    length(Elems, ElemCount),
    handle_streams_type(Handle, _, SW, Type),
    handle_elemcount_bytecount(Handle, ElemCount, ByteCount),
    type_values_bytes_remain(Type, Elems, Bytes, []),
    assertion(length(Bytes, ByteCount)),
    put_bytes(SW, Bytes).

/** db_read(++Handle, +Elems)

The length of Elems must be bound.

@error truncated_last_record(Remain)
if the last record is truncated (its size is not an integer multiple of stride).
This may be due to a previous write error.

@error instantiation_error
if Elems is not bound
*/
db_read(Handle, Elems) :-
    must_be(list, Elems),
    length(Elems, ElemCount),
    handle_streams_type(Handle, SR, _, Type),
    handle_elemcount_bytecount(Handle, ElemCount, ByteCount),
    length(Bytes, ByteCount),
    get_bytes(SR, Bytes),
    type_values_bytes_remain(Type, Elems, Bytes, Remain),
    (Remain = [] -> true ; throw(error(truncated_last_record(Remain), _))).
