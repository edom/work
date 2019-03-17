:- module(my_sgml_write,[
    xml_write/3
    , write_myxml/1
]).

/** <module> monkey-patched SWI-Prolog 7.6.4 library(sgml_write)

The original sgml_write:emit_indent/2 uses tabs
and assumes that 1 level = 2 spaces and 1 tab = 8 spaces.

IntelliJ IDEA auto-detects 1 level = 2 spaces, but assumes 1 level = 1 tab.

Original source:
http://www.swi-prolog.org/pldoc/doc/_SWI_/library/sgml_write.pl?show=src#emit_indent/2
*/

:- use_module(library(sgml_write),[
    xml_write/3
]).

:- abolish(sgml_write:emit_indent/2).

sgml_write:emit_indent(Indent, Out) :-
    format(Out, '~N', []),
    sgml_write:write_n(Indent, ' ', Out).

% ------- Prolog functor represents XML element

write_myxml(Doc) :-
    current_output(Stream),
    myelem_xml(Doc,Xml),
    xml_write(Stream,Xml,[header(true)]), nl.

myelem_xml(A,B) :- string(A), !, A = B.
myelem_xml(A,B) :- A =.. [Name|Conts], !,
    B = element(Name,Attrs,Elems),
    conts_attrs_elems(Conts,Attrs,Elems).

    conts_attrs_elems([],[],[]) :- !.
    conts_attrs_elems([C|C1],As,Es) :- C = [_|_], !,
        conts_attrs_elems(C,A0,E0),
        conts_attrs_elems(C1,A1,E1),
        append(A0,A1,As),
        append(E0,E1,Es).
    conts_attrs_elems([-S|Cs],As,[S|Es]) :- !, conts_attrs_elems(Cs,As,Es).
    conts_attrs_elems([A=B|Cs],[A=B|As],Es) :- !, conts_attrs_elems(Cs,As,Es).
    conts_attrs_elems([C|Cs],As,[E|Es]) :- myelem_xml(C,E), conts_attrs_elems(Cs,As,Es).
