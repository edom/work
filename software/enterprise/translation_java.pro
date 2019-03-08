:- module(translation_java,[
    generate/0
]).
:- use_module('./translation_java_dcg.pro').
:- use_module('./ontology_systems.pro', [
    type_integer_bit/2
    , type_identifier_bit/2
    , type_string/1
    , type_optional/2
    , recordtype/1
    , recordtype_field/2
    , field_name/2 as type_field_name
    , field_type/2 as type_field_type
]).
:- use_module('./ontology_web_applications.pro', [
    state/1
    , page_method/2
    , page_path/2
]).
/** <module> Translation from specification to Java web application

*/

:- style_check(-discontiguous).

% ------- language-user parameters

/** maven_coordinates(?GroupId,?ArtifactId,?Version) is det.
    default_package_name(?PackageName) is det.
    app_dir(?AppDir) is det.

Version is an atom.
Version should have three components.
Each component should be a number.
Example Version: '1.23.456'.

PackageName is an atom.

AppDir is a string without trailing slash.
*/
maven_coordinates('com.spacetimecat.java','example','0.0.0').
default_package_name('com.spacetimecat.java').
app_dir("out").

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

bool(G,B) :- call(G), !, B=true.
bool(_,false).

% ------- language-designer stuff

get_app_dir(A) :- once_no_fail(app_dir(A)).

ensure_app_dir :-
    get_app_dir(Dir),
    print_message(informational,mkdir(Dir)),
    make_directory_path(Dir).

prolog:message(mkdir(Path)) -->
    ['Creating directory ~w'-[Path]].

% Default for all classes.
class(C) :- class_name(C,_).
class_access(_,public).
class_final(_,true).

class_sourcefilepath(C,Path) :-
    get_app_dir(Dir),
    class_packagename(C,Package),
    class_name(C,ClassName),
    split_string(Package,".","",PackageParts),
    string_concat(ClassName,".java",FileName),
    append([[Dir,src,main,java],PackageParts,[FileName]],Parts),
    atomics_to_string(Parts,'/',Path).

% XXX
method_final(_,true).
method_static(_,false).

% ------- some checking

% We can refactor this check into one line
% if we use an RDF-triple-like predicate for everything.
error(missing_property(class,C,name)) :- class(C), \+class_name(C,_).
error(missing_property(class,C,access)) :- class(C), \+class_access(C,_).
error(missing_property(class,C,final)) :- class(C), \+class_final(C,_).
error(missing_property(class,C,packagename)) :- class(C), \+class_packagename(C,_).
error(missing_property(field,F,name)) :- field(F), \+field_name(F,_).
error(missing_property(field,F,type)) :- field(F), \+field_type(F,_).
error(missing_property(method,M,name)) :- method(M), \+method_name(M,_).
error(missing_property(method,M,returntype)) :- method(M), \+method_returntype(M,_).

check :- \+error(_), !.
check :- error(E), print_message(error,E), fail.

prolog:error_message(missing_property(Class,Individual,PropName)) -->
    ["The ~w ~w is missing property ~w"-[Class,Individual,PropName]].

get_package_name(Suffix,Result) :-
    once_no_fail(default_package_name(Base)),
    atomic_list_concat([Base,'.',Suffix],Result).

% ------- map types

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

% ------- translate each record type to a Java entity class

% class-type
ct(C) :- ct(C,_).
ct(recordtype-T, T) :- recordtype(T).

% class-type-field
ctf(C,T,F) :- ct(C,T), recordtype_field(T,F).
ctf(C,T,F,N) :- ctf(C,T,F), type_field_name(F,N).
ctf(C,T,F,N,FT) :- ctf(C,T,F,N), type_field_type(F,FT).

recordtype_class(T,C,Package,Name,Comment) :- ct(C,T),
    get_package_name(entity,Package),
    type_javaclassname(T,Name),
    Comment = generated-from-recordtype-T.

    class_comment(C,K) :- recordtype_class(_,C,_,_,K).
    class_packagename(C,P) :- recordtype_class(_,C,P,_,_).
    class_name(C,N) :- recordtype_class(_,C,_,N,_).
    class_constructor(C,C-defcon) :- ct(C).
        method_access(C-defcon,public) :- ct(C).
        method_parameter(C-defcon,C-defcon-Name) :- ctf(C,_,_,Name).
            parameter_name(C-defcon-Name,Name) :- ctf(C,_,_,Name).
            parameter_type(C-defcon-Name,JType) :- ctf(C,_,_,Name,Type), type_javatype(Type,JType).
        method_statement(C-defcon,100,assign(this:Name,name(Name))) :- ctf(C,_,_,Name).

recordtype_field(T,C,JF,Name,JType) :-
    JF = C-Name,
    ctf(C,T,_,Name,FT),
    type_javatype(FT,JType).

    class_field(C,F) :- recordtype_field(_,C,F,_,_).
        field(F) :- recordtype_field(_,_,F,_,_).
        field_name(F,Name) :- recordtype_field(_,_,F,Name,_).
        field_type(F,Type) :- recordtype_field(_,_,F,_,Type).
        field_access(F,public) :- recordtype_field(_,_,F,_,_).
        field_final(F,true) :- recordtype_field(_,_,F,_,_).

% ------- translate each web application state to Java field in the State class

class_name(state,'State').
class_packagename(state,P) :- get_package_name(app,P).
class_field(state,state-Name) :- state(Name).
    field(state-Name) :- state(Name).
    field_name(state-Name,Name) :- state(Name).
    field_type(state-Name,JT) :- state_type(Name,T), type_javatype(T,JT).
    field_access(state-Name,public) :- state(Name).
    field_initializer(state-Name,Init) :- state_initializer(Name,Init).

% ------- translate each web page to a Java method in the Pages class

class_name(pages,'Pages').
class_packagename(pages,P) :- get_package_name(app,P).
class_method(pages,pages-Page-Method) :- page_method(Page,Method).
    method(pages-Page-Method) :- page_method(Page,Method).
    method_name(pages-Page-Method,Name) :- atomic_list_concat([Method,'_',Page],Name).
    method_returntype(pages-Page-Method,void) :- method_name(pages-Page-Method,_).
    method_access(pages-Page-Method,public) :- method_name(pages-Page-Method,_).

% ------- translate each route to a conditional in the Router class dispatch method

class_name(router,'Router').
class_packagename(router,P) :- get_package_name(app,P).
class_method(router,router-dispatch).
    method(router-dispatch).
    method_name(router-dispatch,dispatch).
    method_returntype(router-dispatch,void).
    method_access(router-dispatch,public).
    method_parameter(router-dispatch,router-dispatch-request).
    method_parameter(router-dispatch,router-dispatch-response).
        parameter_name(router-dispatch-request,request).
        parameter_type(router-dispatch-request,'javax.servlet.http.HttpServletRequest').
        parameter_name(router-dispatch-response,response).
        parameter_type(router-dispatch-response,'javax.servlet.http.HttpServletResponse').
    method_statement(router-dispatch,100,let('java.lang.String', pathInfo, name(request):getPathInfo())).
    method_statement(router-dispatch,200,if(name(request):getPathInfo() == null, return)).
    method_statement(router-dispatch,300,if(SPath:equals(name(pathInfo)), return)) :-
        page_path(_,Path),
        atom_string(Path,SPath).

% ------- write using translation_java_dcg

generate :-
    check,
    ensure_app_dir,
    foreach(class(C), generate_class(C)).

generate_class(Class) :-
    once_no_fail(class_packagename(Class,Package)),
    once_no_fail(class_name(Class,Name)),
    once_no_fail(class_access(Class,Access)),
    once_no_fail(class_final(Class,Final)),
    once_no_fail(class_sourcefilepath(Class,Source)),
    print_message(informational,generate_class(Class,Source)),
    write_phrase(class_begin(Package,Name,Access,Final)),
    foreach(class_field(Class,Field), write_field(Field)),
    foreach(class_constructor(Class,Ctor), write_constructor(Ctor)),
    foreach(class_method(Class,Method), write_method(Method)),
    write_phrase(class_end).

    write_phrase(P) :-
        once(phrase(P,Codes)),
        string_codes(Str,Codes),
        write(Str).

    write_field(Field) :-
        once_no_fail(field_name(Field,Name)),
        once_no_fail(field_type(Field,Type)),
        once_no_fail(field_access(Field,Access)),
        once_no_fail(field_final(Field,Final)),
        (field_initializer(Field,Init)
        ->  write_phrase(field(Name,Type,Access,Final,Init))
        ;   write_phrase(field(Name,Type,Access,Final))
        ).

        field_final(Field,Final) :- bool(field_final(Field),Final).

    write_constructor(K) :-
        class_constructor(C,K),
        once_no_fail(class_name(C,Name)),
        once_no_fail(method_access(K,Access)),
        write("    "),
        write_phrase(access(Access)), write(" "),
        format("~w ",[Name]),
        write_method_parameters(K), write(" "),
        write_method_body(K).

    write_method(M) :-
        once_no_fail(method_access(M,Access)),
        once_no_fail(method_name(M,Name)),
        once_no_fail(method_returntype(M,Ret)),
        once_no_fail(method_static(M,Static)),
        once_no_fail(method_final(M,Final)),
        write("    "),
        write_phrase(access(Access)), write(" "),
        write_phrase(static(Static)), write(" "),
        write_phrase(final(Final)), write(" "),
        format(" ~w ~w ",[Ret,Name]),
        write_method_parameters(M), write(" "),
        write_method_body(M).

    write_method_parameters(M) :-
        write("("),
        findall(P, method_parameter(M,P), Ps),
        do_write_parameters(Ps),
        write(")").

        do_write_parameters([]) :- !.
        do_write_parameters([A]) :- !, write_parameter(A).
        do_write_parameters([A|B]) :- !, write_parameter(A), write(", "), do_write_parameters(B).

            write_parameter(P) :-
                parameter_name(P,Name),
                parameter_type(P,Type),
                format("~w ~w", [Type,Name]).

    write_method_body(M) :-
        format("{\n"),
        % TODO order before printing
        foreach(method_statement(M,_,S), write_phrase(statement(S))),
        format("    }\n").

prolog:message(generate_class(Id,Source)) -->
    ['Writing Java class ~w into ~w'-[Id,Source]].
