:- module(java_util,[
    once_no_fail/1
    , javatype_javareftype/2
    , type_javaclassname/2
    , name_jname/2
    , stringies_concat/2
    , parts_path/2
    , ensure_dir/1
    , with_output_to_file/3
    , statement_normalize/2
    , check_statement/1
]).
/** <module> Internal helper predicates used by java_program_from_web_app.pro

*/

% ------- meta-predicates

/** once_no_fail(:Goal) is det.

If Goal succeeds, this is like once/1.

If Goal fails, this is like throw/1.
*/
:- meta_predicate once_no_fail(0).
once_no_fail(G) :- call(G), !.
once_no_fail(G) :- throw(error(should_not_fail(G),_)).

prolog:error_message(should_not_fail(Goal)) -->
    ['Goal should not fail: ~w\n'-[Goal]],
    ['Hint: Did someone forget or mistype the predicate definition?'].

% ------- types

javatype_javareftype(boolean,R) :- !, R = 'java.lang.Boolean'.
javatype_javareftype(char,R) :- !, R = 'java.lang.Character'.
javatype_javareftype(void,R) :- !, R = 'java.lang.Void'.
javatype_javareftype(byte,R) :- !, R = 'java.lang.Integer'.
javatype_javareftype(short,R) :- !, R = 'java.lang.Short'.
javatype_javareftype(int,R) :- !, R = 'java.lang.Integer'.
javatype_javareftype(long,R) :- !, R = 'java.lang.Long'.
javatype_javareftype(float,R) :- !, R = 'java.lang.Float'.
javatype_javareftype(double,R) :- !, R = 'java.lang.Double'.
javatype_javareftype(A,A).

type_javaclassname(T,J) :-
    atom_codes(T,C),
    capitalize(C,Cap),
    atom_codes(J,Cap).

    capitalize([H|T],[H0|T]) :- code_type(H0,to_upper(H)).
    capitalize([],[]).

% ------- map Prolog terms to Java names

name_jname(Name,JName) :-
    term_string(Name,SName,[quoted(false)]),
    string_codes(SName,CName),
    codes_javanamecodes(CName,CJName),
    string_codes(SJName,CJName),
    atom_string(JName,SJName).

stringies_concat([],"") :- !.
stringies_concat([A|B],Z) :- !,
    stringy_string(A,A0),
    stringies_concat(B,B0),
    string_concat(A0,B0,Z).

    stringy_string(A,B) :- term_string(A,B,[quoted(false)]).

    codes_javanamecodes([],[]) :- !.
    codes_javanamecodes([0'-|B],[0'_|C]) :- !, codes_javanamecodes(B,C).
    codes_javanamecodes([A|B],[A|C]) :- codes_javanamecodes(B,C).

% ------- mapping file paths

parts_path([],"") :- !.
parts_path([A],Z) :- !, term_string(A,Z,[quoted(false)]).
parts_path([A|B],Z) :- !,
    term_string(A,A0,[quoted(false)]),
    parts_path(B,B0),
    string_concat(A0,"/",A1),
    string_concat(A1,B0,Z).

ensure_dir(Dir) :-
    make_directory_path(Dir).

:- meta_predicate with_output_to_file(+,+,0).
with_output_to_file(Path,Opts,Goal) :-
    file_directory_name(Path,Dir),
    ensure_dir(Dir),
    setup_call_cleanup(
        open(Path,write,Stream,Opts),
        with_output_to(Stream,Goal),
        close(Stream)
    ).

% ------- normalization, checking, and inference

/** statement_normalize(+Stm,-NStm) is det.
    expression_normalize(+Exp,-NExp) is det.
*/
statement_normalize(A,_) :- var(A), !, instantiation_error(A).
statement_normalize(return,Z) :- !, Z = return.
statement_normalize(let(T,N,E),Z) :- !, Z = let(T,N,E0), expression_normalize(E,E0).
statement_normalize(if(A,B),Z) :- !, Z = if(A0,B0),
    expression_normalize(A,A0),
    statement_normalize(B,B0).
statement_normalize(if(A,B,C),Z) :- !, Z = if(A0,B0,C0),
    expression_normalize(A,A0),
    statement_normalize(B,B0),
    statement_normalize(C,C0).
statement_normalize(A:=B,C) :- !, C = assign(A,B0), expression_normalize(B,B0).
statement_normalize(A,B) :- expression(A), !, expression_normalize(A,B).
statement_normalize(A,_) :-  domain_error(java_statement_convenient,A).

expression(E) :- string(E), !.
expression(E) :-
    functor(E,Name,Arity),
    member(Name/Arity, [
        this/0
        , null/0
        , name/1
        , new/1
        , new/2
        , ':'/2
        , ':='/2
        , '=='/2
        , field/2
    ]).

expression_normalize(A,_) :- var(A), !, instantiation_error(A).
expression_normalize(A,Z) :- string(A), !, Z = A.
expression_normalize(A,Z) :- number(A), !, Z = int(A).
expression_normalize(this,Z) :- !, Z = this.
expression_normalize(null,Z) :- !, Z = null.
expression_normalize(field(E,N),Z) :- !, Z = field(A0,N), expression_normalize(E,A0).
expression_normalize(name(A),B) :- !, B = name(A).
expression_normalize(class(A),Z) :- !, Z = class(A).
expression_normalize(new(T),N) :- !, expression_normalize(new(T,[]),N).
expression_normalize(new(T,A),N) :- !, N = new(T,A0), expression_normalize1(A,A0).
expression_normalize(\+A,N) :- !, N = not(A0), expression_normalize(A,A0).
expression_normalize(A/\B,N) :- !, N = and(A0,B0),
    expression_normalize(A,A0),
    expression_normalize(B,B0).
expression_normalize(A\/B,N) :- !, N = or(A0,B0),
    expression_normalize(A,A0),
    expression_normalize(B,B0).
expression_normalize(A:B,Z) :- !,
    B =.. [Name|Args],
    expression_normalize(call(A,Name,Args),Z).
expression_normalize(A:=B,N) :- !, N = assign(A0,B0),
    expression_normalize(A,A0),
    expression_normalize(B,B0).
expression_normalize(A==B,N) :- !, N = equal(A0,B0),
    expression_normalize(A,A0),
    expression_normalize(B,B0).
expression_normalize($A,Z) :- !, Z = name(A).
expression_normalize(call(T,N,A),Z) :- !, Z = call(T0,N,A0),
    expression_normalize(T,T0),
    expression_normalize1(A,A0).
expression_normalize(A,_) :- domain_error(java_expression_convenient,A).

    expression_normalize1([],[]) :- !.
    expression_normalize1([A|B],[A0|B0]) :- !, expression_normalize(A,A0), expression_normalize1(B,B0).

% This infers types in some simple cases.
check_statement(let(T,_,E)) :- expression_type(E,T), !.
check_statement(_).

expression_type(new(T,_),T).
