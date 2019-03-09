:- module(translation_java,[]).
:- use_module('./ontology_systems.pro', [
    type_integer_bit/2
    , type_identifier_bit/2
    , type_string/1
    , type_optional/2
    , recordtype/1
]).
/** <module> Translate each web application to Java

Translate from ontology_web_applications.pro to ontology_java_programs.pro.
*/



% ------- begin customization section of language-user parameters

/** default_package_name(?PackageName) is det.

PackageName is an atom.
*/
:- multifile default_package_name/1.

% ------- end customization section



% ------- imports

/** recordtype_field(?TypeId,?FieldId,?FieldName,?FieldType) is nondet.

Imports.
*/
:- multifile recordtype_field/4,
             webapp/1,
             webapp_state/2,
             webapp_page/2,
             state/1,
             state_type/2,
             state_initializer/2,
             page_method/2,
             page_path/2.

% ------- presenting internal structure as ontology_java_programs.pro structure

program_class(P,C) :- java_program_class(P,C,_,_,_).

class(C) :- java_program_class(_,C,_,_,_).
class_package_name(A,B) :- java_program_class(_,A,B,_,_).
class_name(A,B) :- java_program_class(_,A,_,B,_).
class_comment(C,K) :- java_program_class(_,C,_,_,L), member(comment-K,L).

class_field(A,B) :- java_class_field(A,B,_,_,_).
class_method(A,B) :- java_class_method(A,B,_,_,_).

field(A) :- java_class_field(_,A,_,_,_).
field_name(A,B) :- java_class_field(_,A,B,_,_).
field_type(A,B) :- java_class_field(_,A,_,B,_).
field_initializer(A,B) :-
    java_class_field(_,A,_,_,L),
    (member(initializer-I,L)
    ->  B = some(I)
    ;   B = none).

method(A) :- java_class_method(_,A,_,_,_).
method_name(A,B) :- java_class_method(_,A,B,_,_).
method_return_type(A,B) :- java_class_method(_,A,_,B,_).

element_access(A,public) :- element_option(A,public), !.
element_access(A,protected) :- element_option(A,protected), !.
element_access(A,package) :- element_option(A,package), !.
element_access(A,private) :- element_option(A,private), !.
element_access(_,public).

element_static(A,true) :- element_option(A,static), !.
element_static(_,false).

element_final(A,true) :- element_option(A,final), !.
element_final(_,false).

    element_options(A,B) :- java_program_class(_,A,_,_,B).
    element_options(A,B) :- java_class_method(_,A,_,_,B).
    element_options(A,B) :- java_class_field(_,A,_,_,B).

    element_option(A,B) :- element_options(A,L), member(B,L).

:- discontiguous method_parameter/2,
                 method_statement/3,
                 parameter_name/2,
                 parameter_type/2.

% ------- internal structure

/** java_program_class(?ProgId,?ClassId,?Package,?Name,?ClassOpts) is nondet.
    java_class_field(?ClassId,?FieldId,?FieldName,?JavaType,?FieldOpts) is nondet.
    java_class_method(?ClassId,?MethodId,?MethodName,?ReturnType,?MethodOpts) is nondet.

Internal database inferred from rules.
*/
:- discontiguous java_program_class/5,
                 java_class_field/5,
                 java_class_method/5.

% ------- programs

webapp_program(A,webapp-A) :- webapp(A).

program(P) :- webapp_program(_,P).
program_maven_coordinates(P,'com.spacetimecat.java',A,'0.0.0') :- webapp_program(A,P).

% ------- package naming logic

compute_package(literal(A),Z) :- !, A = Z.
compute_package(default,Z) :- !, once_no_fail(default_package_name(Z)).
compute_package(program(webapp-P),Z) :- !,
    once_no_fail(program(webapp-P)),
    compute_package(default:literal(P),Z).
compute_package(entity(P),Z) :- !, compute_package(program(P):literal(entity),Z).
compute_package(app(P),Z) :- !, compute_package(program(P):literal(app),Z).
compute_package(A:B,C) :- !,
    compute_package(A,A0),
    compute_package(B,B0),
    atomic_list_concat([A0,'.',B0],C).
compute_package(A,_) :- domain_error(package_expression,A).

% ------- meta-predicates

/** once_no_fail(:Goal) is det.

If Goal succeeds, this is like once/1.

If Goal fails, this is like throw/1.
*/
once_no_fail(G) :- call(G), !.
once_no_fail(G) :- throw(error(should_not_fail(G),_)).

prolog:error_message(should_not_fail(Goal)) -->
    ['Goal should not fail: ~w\n'-[Goal]],
    ['Hint: Did someone forget or mistype the predicate definition?'].

% ------- map from types to Java types

type_javatype(T,J) :- type_integer_bit(T,N), 0 =< N, N =< 32, !, J = int.
type_javatype(T,J) :- type_integer_bit(T,N), 32 < N, N =< 64, !, J = long.
type_javatype(T,J) :- type_identifier_bit(T,N), 0 =< N, N =< 32, !, J = int.
type_javatype(T,J) :- type_identifier_bit(T,N), 32 < N, N =< 64, !, J = long.
type_javatype(T,J) :- type_string(T), !, J = 'java.lang.String'.
type_javatype(T,J) :- type_optional(T,A), !, type_javatype(A,P), javatype_javareftype(P,J).
type_javatype(T,_) :- throw(error(no_related_java_type_for_type(T),_)).

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

% ------- main class

java_program_class(Prog,Prog-main,Package,'Main',[final]) :-
    program(Prog),
    compute_package(app(Prog),Package).

    java_class_method(Prog-main,Prog-main-main,main,void,[public,static]) :-
        program_class(Prog,Prog-main).
    method_statement(Prog-main-main,100,name('System.out'):println("Hello")) :-
        program_class(Prog,Prog-main).

% ------- translate each record type to a Java entity class

% TODO program-class-type

% class-type
ct(recordtype-T, T) :- recordtype(T).
ct(C) :- ct(C,_).

% class-type-field
ctf(C,T,F,FN,FT) :- ct(C,T), recordtype_field(T,F,FN,FT).
ctf(C,T,F,FN) :- ctf(C,T,F,FN,_).
ctf(C,T,F) :- ctf(C,T,F,_).

java_program_class(Prog,C,Package,Name,[final,comment-(generated-from-recordtype-T)]) :-
    program(Prog),
    ct(C,T),
    compute_package(entity(Prog),Package),
    type_javaclassname(T,Name).

    class_constructor(C,C-defcon) :- ct(C).
        method_parameter(C-defcon,C-defcon-Name) :- ctf(C,_,_,Name).
            parameter_name(C-defcon-Name,Name) :- ctf(C,_,_,Name).
            parameter_type(C-defcon-Name,JType) :- ctf(C,_,_,Name,Type), type_javatype(Type,JType).
        method_statement(C-defcon,100,assign(field(this,Name),name(Name))) :- ctf(C,_,_,Name).

java_class_field(C,C-Name,Name,JT,[final]) :-
    ctf(C,_,_,Name,FT),
    type_javatype(FT,JT).

% ------- translate each web application state to Java field in the State class

java_program_class(Prog,Prog-state,Package,'State',[final]) :-
    program(Prog),
    compute_package(app(Prog),Package).

java_class_field(Prog-state,Prog-state-Name,JName,JT,Init) :-
    webapp_program(A,Prog),
    webapp_state(A,Name),
    once_no_fail(state_type(Name,T)),
    name_jname(Name,JName),
    type_javatype(T,JT),
    (state_initializer(Name,Init0)
    ->  Init = [initializer-Init0]
    ;   Init = []).

% ------- translate each web page to a Java method in the Pages class

java_program_class(Prog,Prog-pages,Package,'Pages',[final]) :-
    program(Prog),
    compute_package(app(Prog),Package).

java_class_method(Prog-pages,Prog-pages-Page-HttpMethod,JName,void,[]) :-
    webapp_program(W,Prog),
    webapp_page(W,Page),
    page_method(Page,HttpMethod),
    stringies_concat([HttpMethod,'_',Page],SName),
    name_jname(SName,JName).

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

% ------- translate each route to a conditional in the Router class dispatch method

java_program_class(Prog,Prog-router,Package,'Router',[final]) :-
    program(Prog),
    compute_package(app(Prog),Package).

java_class_method(Prog-router,Prog-router-dispatch,dispatch,void,[]) :- program(Prog).
    method_parameter(Prog-router-dispatch,Prog-router-dispatch-request) :- program(Prog).
    method_parameter(Prog-router-dispatch,Prog-router-dispatch-response) :- program(Prog).
        parameter_name(Prog-router-dispatch-request,request) :- program(Prog).
        parameter_type(Prog-router-dispatch-request,'javax.servlet.http.HttpServletRequest') :- program(Prog).
        parameter_name(Prog-router-dispatch-response,response) :- program(Prog).
        parameter_type(Prog-router-dispatch-response,'javax.servlet.http.HttpServletResponse') :- program(Prog).
    method_statement(Prog-router-dispatch,100,let('java.lang.String', pathInfo, name(request):getPathInfo)) :- program(Prog).
    method_statement(Prog-router-dispatch,200,if(name(pathInfo) == null, return)) :- program(Prog).
    method_statement(Prog-router-dispatch,300,if(SPath:equals(name(pathInfo)), return)) :-
        webapp_program(A,Prog),
        webapp_page(A,Page),
        page_path(Page,Path),
        atom_string(Path,SPath).
