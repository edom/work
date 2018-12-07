:- module(database_internal_dcg, [
    type_value//2,
    type_bytecount/2,
    type_value_bytes/3,
    type_values_bytes_remain/4
]).
:- use_module(library(clpfd)).
:- use_module(library(utf8)).
:- use_module('./ps1_bit.pro').
/** <module> structured-data layout model

Similar things:
    - ASN.1
    - Protobuf, Thrift, Avro
    - COBOL PICTURE clause, Pascal record, C struct

Questions:
    - Should we just use an existing solution?
    - Should we Embed some schema / type information in the wire format?
    - Big-endian?
*/

/** type_value(++Type, ?Value)// is nondet.

This is a bidirectional DCG rule for parsing byte list to database values or the other way around.

Type must be ground and acyclic.

Type may be any of these:
    - fixed-length integers that fit in a CPU register: Value must be an integer
        - =u8=, =u16=, =u32=, =u64=: unsigned integer in little-endian byte order
        - =s8=, =s16=, =s32=, =s64=: two's complement signed integer in little-endian byte order
    - variable-but-limited-length character strings: Value must be a double-quoted string
        - vchars1(N), vchars2(N), vchars4(N): a leading length integer, followed by a UTF-8 encoded character string
            - N is the maximum number of _bytes_ (not characters) in the string, excluding the leading length integer.
            Thus the number of bytes in Value must not exceed N.
            - The leading length integer counts _bytes_, not characters.
            - vchars1 uses a u8 length.
            - vchars2 uses a u16 length.
            - vchars4 uses a u32 length.
            - A vcharsL(N) takes up N+L bytes of storage regardless of its effective length.
            The content is zero-padded to make it L bytes long.
    - byte strings: Value must be a list of integers, each between 0 and 255 inclusive
        - bytes(N): fixed-length byte array; the length of Value must be N
        - reserved(N): same as bytes(N), but indicates possible future expansion
        - vbytes1(N), vbytes2(N), vbytes4(N): like vchars, but byte string instead of character string; no encoding
    - fixed-length sequences: Value must be a list
        - =|[T1, ..., Tn]|= where each of T1, ..., Tn is a type: this list represents a sequence.
    - fixed-length records: Value must be a compound term
        - functor(Name, ArgTypes), where Name is an atom, and ArgTypes is a list of types
            - The name of Value must be Name
            - The arity of Value must be the length of ArgTypes
    - TODO?
        - fixed(N,T): type T but limited to N bytes; storage size is fixed N bytes
        - align(A,T)
        - Replace vcharsL(N) with fixed(N,vcharsL(Max))
        - funlist(Name, ArgTypes)
            - Example: [id(1), name("dog"), sound("bark")]
        - call(P): P describes a predicate of arity 2: P(Value, Bytes)
        - calldcg(R): R describes a DCG rule of arity 1 (predicate of arity 3): R(Value)//
        - vsint1(N): variable-length signed integer; this is the internal format used by GNU MP library?

There are no vbytes8 and vchars8.
This library is not designed for things that big.
Don't use this library user to store things that don't fit in the RAM.

If Type is a list, then Value must be a list of the same length.
(TODO throw an error if this is violated.)

Example:
```
?- % animal(Id, Name, Sound)
Type = functor(animal, [u32, vchars1(27), vchars1(31)]),
Val = animal(1, "dog", "woof"),
type_value_bytes(Type, Val, Bytes), length(Bytes, Length), type_value_bytes(Type, Readback, Bytes).
Val = Readback, Readback = animal(1, "dog", "woof"),
Bytes = [1, 0, 0, 0, 3, 100, 111, 103, 0|...],
Length = 64.
```

See also:
    - https://en.wikipedia.org/wiki/Data_type
*/
type_value(u8,U) --> [U].
type_value(u16,U) --> {length(Bs,2)}, Bs, {bytesle_uint(Bs,U)}.
type_value(u32,U) --> {length(Bs,4)}, Bs, {bytesle_uint(Bs,U)}.
type_value(u64,U) --> {length(Bs,8)}, Bs, {bytesle_uint(Bs,U)}.
type_value(vbytes1(Max),Bs) --> {pad(Len,Bs,_,_,Max,Tot)}, type_value(u8,Len), Tot.
type_value(vbytes2(Max),Bs) --> {pad(Len,Bs,_,_,Max,Tot)}, type_value(u16,Len), Tot.
type_value(vbytes4(Max),Bs) --> {pad(Len,Bs,_,_,Max,Tot)}, type_value(u32,Len), Tot.
type_value(vchars1(Max),S) --> {clp_string_utf8(S,B)}, type_value(vbytes1(Max),B).
type_value(vchars2(Max),S) --> {clp_string_utf8(S,B)}, type_value(vbytes2(Max),B).
type_value(vchars4(Max),S) --> {clp_string_utf8(S,B)}, type_value(vbytes4(Max),B).
type_value(bytes(N),Bs) --> {length(Bs,N)}, Bs.
type_value(reserved(N),Bs) --> type_value(bytes(N),Bs).
type_value(functor(Name,ArgTypes),V) -->
    {length(ArgTypes,Arity), length(ArgVals,Arity), V =.. [Name|ArgVals]},
    type_value(ArgTypes,ArgVals).
type_value([],[]) --> [].
type_value([H|T],[HV|TV]) --> type_value(H,HV), type_value(T,TV).

/** type_bytecount(Type, Count)

"Storing an instance of Type requires Count bytes."
*/
type_bytecount(u8, 1).
type_bytecount(u16, 2).
type_bytecount(u32, 4).
type_bytecount(u64, 8).
type_bytecount(bytes(N), N).
type_bytecount(reserved(N), N).
type_bytecount(vbytes1(L), LL) :- LL is L + 1.
type_bytecount(vbytes2(L), LL) :- LL is L + 2.
type_bytecount(vbytes4(L), LL) :- LL is L + 4.
type_bytecount(vchars1(L), LL) :- LL is L + 1.
type_bytecount(vchars2(L), LL) :- LL is L + 2.
type_bytecount(vchars4(L), LL) :- LL is L + 4.
type_bytecount(functor(_,As), L) :- type_bytecount(As, L).
type_bytecount([], 0).
type_bytecount([H|T], S) :- S #= SH+ST, type_bytecount(H,SH), type_bytecount(T,ST).

/** type_value_bytes(++Type, ?Value, ?Bytes)

"Type serializes Value to Bytes."

This is a convenience predicate that calls phrase/2.

Parameter requirements:
    - Type must be ground and acyclic.
    - Value or Bytes or both must be bound.
*/
type_value_bytes(Type, Value, Bytes) :- phrase(type_value(Type,Value), Bytes).

type_values_bytes_remain(Type, [V|Vs], Bytes, Remain) :-
    phrase(type_value(Type,V), Bytes, Rem0), !,
    type_values_bytes_remain(Type, Vs, Rem0, Remain).
type_values_bytes_remain(_, [], Bs, Bs).

/** pad(InputLength, Input, PaddingLength, Padding, ConcatLength, Concat)

Add or remove padding.

There are two ways to use this predicate:
    - Given Input and ConcatLength, compute Concat.
    - Given InputLength and Concat, compute Input.
*/
pad(Length, Input, Pad, Tail, Limit, Padded) :-
    0 #=< Length,
    0 #=< Pad,
    Limit #= Length + Pad,
    clp_list_length(Padded, Limit),
    clp_list_length(Input, Length),
    clp_list_length(Tail, Pad),
    clp_list_append(Input, Tail, Padded),
    when(ground(Input), default_zero(Tail)).

/**
clp_list_append(?Left, ?Right, ?Both).
clp_list_length(?List, ?Length).
clp_string_utf8(?String, ?Bytes).

Some constraint logic programming.
*/
clp_list_append(A,B,C) :- when( (nonvar(A),nonvar(B),nonvar(C)), append(A,B,C) ).
clp_list_length(L,N) :- when( (nonvar(L);ground(N)), length(L,N) ).
clp_string_utf8(S,B) :- when( (ground(S);ground(B)), string_utf8(S,B) ).

/** default_zero(?List)

Unify every variable element of List with zero.
*/
default_zero([]).
default_zero([0|T]) :- !, default_zero(T).
default_zero([_|T]) :- default_zero(T).

/** string_utf8(?String, ?Bytes)

"Encoding String in UTF-8 produces Bytes."

String is a double-quoted string.

Bytes is a list of integers.

This is bidirectional.
At least one argument must be ground.
*/
string_utf8(Str, Bytes) :- nonvar(Str), !, string_codes(Str, Codes), phrase(utf8_codes(Codes), Bytes).
string_utf8(Str, Bytes) :- nonvar(Bytes), !, phrase(utf8_codes(Codes), Bytes), string_codes(Str, Codes).
string_utf8(S,B) :- instantiation_error(string_utf8(S,B)).
