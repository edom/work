:- module(java_writer_dcg,[
    class_begin//6
    , class_end//0
    , field//6
    , statement//1
    , expression//1
    , access//1
    , final//1
    , static//1
    , type//1
    , name//1
    , throws//1
]).
/** <module> Unparsing-only definite-clause grammar for Java

This is internal to java_program_from_web_app.pro.

The DCG is left-recursive, so its naive usage is only possible for unparsing, not parsing.

The inability to use the same grammar for both parsing and unparsing is inconvenient but not fatal.

Known problem: Ugly output.
The Java compiler does not care about whitespaces.
But humans care about whitespaces when they check the generated code.
*/

access(public) --> !, "public ".
access(protected) --> !, "protected ".
access(package) --> !, "".
access(private) --> !, "private ".
access(A) --> {domain_error(java_access_modifier,A)}.

final(true) --> !, "final ".
final(false) --> !, "".
final(A) --> {domain_error(boolean,A)}.

static(true) --> !, "static ".
static(false) --> !, "".
static(A) --> {domain_error(boolean,A)}.

type(A) --> {\+ground(A), !, instantiation_error(A)}.
type(array(A)) --> !, type(A), "[]".
type(A) --> {atom(A), !, atom_codes(A,C)}, C.
type(A) --> {domain_error(java_type,A)}.

name(A) --> {must_be(atom,A), atom_codes(A,C)}, C.

throws([]) --> !, "".
throws(A) --> !, "throws ", types(A).

types([]) --> !, "".
types([A]) --> !, type(A).
types([A|B]) --> !, type(A), ", ", types(B).

class_begin(Package,Name,Access,Final,OptSuperClassType,IfaceTypeList) -->
    {atom_codes(Package,CP), atom_codes(Name,CN)},
    "package ", CP, ";\n\n",
    access(Access), final(Final), "class ", CN,
    extends(OptSuperClassType),
    implements(IfaceTypeList),
    " {\n".

extends(none) --> !, "".
extends(some(T)) --> !, " extends ", type(T).

implements([]) --> !, "".
implements(A) --> !, " implements ", types(A).

class_end --> "}\n".

field(Name,Type,Access,Static,Final,Init) -->
    indent(4),
    access(Access),
    static(Static),
    final(Final),
    type(Type), " ",
    name(Name), initializer(Init),
    ";\n".

initializer(none) --> !, "".
initializer(some(Init)) --> !, " = ", expression(Init).

methodbody([]) --> !.
methodbody([A|B]) --> !, statement(A), methodbody(B).

indent(0) --> !, "".
indent(N) --> " ", {N1 is N-1}, indent(N1).

statement(let(T,N,E)) --> !, indent(8), "final ", type(T), " ", name(N), " = ", expression(E), ";\n".
statement(if(C,B)) --> !, indent(8), "if (", expression(C), ") ", statement(B), "\n".
statement(return) --> !, "return;".
statement(E) --> indent(8), stmexp(E), !, ";\n".
statement(S) --> {domain_error(java_statement,S)}.

stmexp(call(T,Name,Args)) --> !, methodcall(T,Name,Args).
stmexp(new(T,A)) --> !, "new ", type(T), arglist(A).
stmexp(assign(L,R)) --> !, expression(L), " = ", expression(R).

% TODO avoid unnecessary parenthesization
exp_order(S,0) :- string(S).
exp_order(null,0).
exp_order(this,0).
exp_order(name(_),0).
exp_order(call(_,_,_),100).
exp_order(equal(_,_),500).
exp_order(assign(_,_),1000).

expression(S) --> {string(S), !, string_codes(S,C), escape(C,E)}, [0'"], E, [0'"].
expression(this) --> !, "this".
expression(null) --> !, "null".
expression(class(A)) --> !, type(A), ".class".
expression(int(A)) --> !, {number_codes(A,C)}, C.
expression(name(N)) --> !, name(N).
expression(not(A)) --> !, "!(", expression(A), ")".
expression(field(T,F)) --> !, fieldaccess(T,F).
expression(equal(A,B)) --> !, binop("==",A,B).
expression(and(A,B)) --> !, binop("&&",A,B).
expression(or(A,B)) --> !, binop("||",A,B).
expression(E) --> stmexp(E), !.
expression(E) --> {domain_error(java_expression,E)}.

    binop(Op,A,B) --> "(", expression(A), ") ", Op, " (", expression(B), ")".

    fieldaccess(this,N) --> !, "this.", name(N).
    fieldaccess(O,N) --> !, "(", expression(O), ").", name(N).

    methodcall(name(T),Name,Args) --> !, name(T), ".", name(Name), arglist(Args).
    methodcall(T,Name,Args) --> "(", expression(T), ").", name(Name), arglist(Args).

    arglist(A) --> "(", arglist_inner(A), ")".

    arglist_inner([]) --> "".
    arglist_inner([A]) --> expression(A).
    arglist_inner([A|B]) --> expression(A), ", ", arglist_inner(B).

    escape([],[]) :- !.
    escape([0'"|T],[0'\\,0'"|T0]) :- !, escape(T,T0).
    escape([0'\n|T],[0'\\,0'n|T0]) :- !, escape(T,T0).
    escape([H|T],[H|T0]) :- !, escape(T,T0).
