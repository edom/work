:- annotate(file,[
    purpose-"read JVM class file",
    tags-[
        java
        , jvm
    ]
]).
:- exports([
    classfilepath_class/2
    , class//1
    , repeat//3
    , bytes//2
    , u1//1
    , u2//1
    , u4//1
]).
:- use_module(library(pure_input),[
    phrase_from_file/3
]).
:- use_module('./ps1_bit.pro',[
]).
/** <module> JVM class file?

The main export of this module is classfilepath_class/2, powered by the DCG rule class//1.

Problems:
    - syntax_error//1 pops up a window.

Should be moved to other modules:
    - DCG rules for binary parsing:
        - u1//1
        - u2//1
        - u4//1
        - bytes//2
    - Somewhat impure DCG rules:
        - repeat//3

2018-12-07:
The plan is to wait for the next stable release of SWI-Prolog after 7.6.4.
I want to use zip.pl to open JAR files, but zip.pl is not in stable.
But when is the next stable release?
The changelog suggested that 7.6.4 was released a year about 7.6.3.
*/

/** classfilepath_class(+Path, -Class)

"Parsing the class file at Path produces Class."

The shape of Class is defined in class//1.
*/
classfilepath_class(Path, Class) :-
    phrase_from_file(class(Class), Path, [type(binary)]).

/** class(?Class)// is nondet.

"Class is a parsed class file."

Class is a list of functors:
    - minor(Min), major(Maj): class file format minor and major version
    - access(A): class access flags/modifiers
    - cpi_this(C): constant pool index of the descriptor of this class
    - cpi_super(C): constant pool index of the descriptor of the superclass of this class
    - interface_indexes(L): constant pool indexes of interfaces implemented by this class
    - constant_pool(L)
    - fields(L): list of Field_info structures; L is a list of functors:
        - access(A)
        - cpi_name(C)
        - cpi_descriptor(C)
        - attributes(L)
    - methods(L): list of Method_info structures; L has the same shape as the list described in fields(L)
    - attributes(L): list of Attribute_info structures

This DCG rule can be used to both parse and unparse a class file.

More information:
    - Wikipedia summary of class file format https://en.wikipedia.org/wiki/Java_class_file
    - Specification of the class file format https://docs.oracle.com/javase/specs/jvms/se8/html/index.html
*/
class(Class) -->
    % This enables the user to permute the functors when unparsing.
    % Parsing produces the functors in this order.
    {
        member(minor(VerMin), Class),
        member(major(VerMaj), Class),
        member(constant_pool_max_index_plus_one(Cpmip1), Class),
        member(constant_pool(Cp), Class),
        member(access(Access), Class),
        member(cpi_this(CpiThis), Class),
        member(cpi_super(CpiSuper), Class),
        member(interface_count(IfaceCount), Class),
        member(interface_indexes(IfaceRefs), Class),
        member(field_count(FieldCount), Class),
        member(fields(Fields), Class),
        member(method_count(MethodCount), Class),
        member(methods(Methods), Class),
        member(attribute_count(AttrCount), Class),
        member(attributes(Attributes), Class),
        length(Class, _), ! % We want the shortest proper the list.
    },
    magic,
    u2(VerMin),
    u2(VerMaj),
    u2(Cpmip1), % constant pool maximum index plus one
    {EntryCount is Cpmip1 - 1}, const_pool(EntryCount, Cp), !,
    u2(Access),
    u2(CpiThis),
    u2(CpiSuper),
    u2(IfaceCount), repeat(IfaceCount, u2, IfaceRefs), !,
    u2(FieldCount), repeat(FieldCount, field_or_method_info, Fields), !,
    u2(MethodCount), repeat(MethodCount, field_or_method_info, Methods), !,
    u2(AttrCount), repeat(AttrCount, attribute_info, Attributes), !.

magic --> [0xca,0xfe,0xba,0xbe].
magic --> [A,B,C,D], syntax_error(invalid_magic([A,B,C,D])).
magic --> syntax_error(unexpected_eof).

const_pool(0, []) --> [].
const_pool(SlotCount, [E|Ents]) -->
    {SlotCount > 0},
    cp_entry(OccupiedSlotCount, E),
    {SlotCount1 is SlotCount - OccupiedSlotCount},
    const_pool(SlotCount1, Ents).

cp_entry(1, mod_utf(L)) --> [1], u2(N), {length(L,N)}, L.
cp_entry(1, int(Val)) --> [3], bytes(4,Val). % TODO
cp_entry(1, float(Val)) --> [4], bytes(4,Val). % TODO
cp_entry(2, long(Val)) --> [5], bytes(8,Val). % TODO
cp_entry(2, double(Val)) --> [6], bytes(8,Val). % TODO
cp_entry(1, cls_ref(ModUtf)) --> [7], u2(ModUtf).
cp_entry(1, str_ref(ModUtf)) --> [8], u2(ModUtf).
cp_entry(1, field_ref(ClsRef,NamTyp)) --> [9], u2(ClsRef), u2(NamTyp).
cp_entry(1, method_ref(ClsRef,NamTyp)) --> [10], u2(ClsRef), u2(NamTyp).
cp_entry(1, iface_method_ref(IfaRef,NamTyp)) --> [11], u2(IfaRef), u2(NamTyp).
cp_entry(1, name_type(ModUtfName,ModUtfType)) --> [12], u2(ModUtfName), u2(ModUtfType).
cp_entry(1, method_handle(What)) --> [15], bytes(3,What).
cp_entry(1, invokedynamic(What)) --> [18], bytes(4,What).
cp_entry(1, method_type(T)) --> [16], u2(T).
cp_entry(_, _) --> [Tag], syntax_error(cp_entry_invalid_tag(Tag)).

field_or_method_info(Info) -->
    {
        member(access(A),Info),
        member(cpi_name(N),Info),
        member(cpi_descriptor(D),Info),
        member(attribute_count(NA),Info),
        member(attributes(As),Info),
        length(Info,_), !
    },
    u2(A), u2(N), u2(D), u2(NA), repeat(NA, attribute_info, As).

attribute_info(Info) -->
    {
        member(cpi_name(N),Info),
        member(length(Len),Info),
        member(bytes(Bs),Info),
        length(Info,_), !
    },
    u2(N), u4(Len), bytes(Len, Bs).

/*
    Somewhat impure DCG rules
*/

/** repeat(?Count, ?DcgGoal, ?Out)// is nondet.

"Repeat DcgGoal Count times, and concatenate the outputs as Out."

Equivalent DCG fragment:
```
DcgGoal(Out1), DcgGoal(Out2), ..., DcgGoal(OutCount), {Out = [Out1, Out2, ..., OutCount]}
```

@tbd Test this metapredicate with goals from different modules.
*/
repeat(0, _, []) --> [].
repeat(Count, Goal, [H|Out]) --> call(Goal,H), {N is Count-1}, repeat(N, Goal, Out).

/** bytes(?Count, ?Bytes)// is nondet.

"Bytes is a list of Count bytes."
*/
bytes(Count, Bytes) --> {length(Bytes, Count)}, Bytes.

u1(Val) --> [Val].
/**
u1(?Value)// is nondet.
u2(?Value)// is nondet.
u4(?Value)// is nondet.

"Value is an integer in big-endian byte order."
*/
u2(Val) --> [B1,B0], {bytesle_uint([B0,B1], Val)}.
u4(Val) --> [B3,B2,B1,B0], {bytesle_uint([B0,B1,B2,B3], Val)}.
