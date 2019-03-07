/** <module> Translation from specification to Java web application

*/

:- multifile
    recordtype/1
    , recordtype_field/2
    , type_field_name/2
    , type_field_type/2
    .

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

% class-type
ct(C) :- ct(C,_).
ct(recordtype-T, T) :- recordtype(T).

% class-type-field
ctf(C,T,F) :- ct(C,T), recordtype_field(T,F).
ctf(C,T,F,N) :- ctf(C,T,F), type_field_name(F,N).
ctf(C,T,F,N,FT) :- ctf(C,T,F,N), type_field_type(F,FT).

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
class_final(_).

class_sourcefilepath(C,Path) :-
    get_app_dir(Dir),
    class_packagename(C,Package),
    class_name(C,ClassName),
    split_string(Package,".","",PackageParts),
    string_concat(ClassName,".java",FileName),
    append([[Dir,src,main,java],PackageParts,[FileName]],Parts),
    atomics_to_string(Parts,'/',Path).

% ------- translate each record type to a Java entity class

get_package_name(Suffix,Result) :-
    once_no_fail(default_package_name(Base)),
    atomic_list_concat([Base,'.',Suffix],Result).

class_comment(C,generated-from-recordtype-T) :- ct(C,T).
class_packagename(C,P) :- ct(C), get_package_name(entity,P).
class_name(C,N) :- ct(C,T), typename_javaclassname(T,N).
class_constructor(C,C-defcon) :- ct(C).
    method_access(C-defcon,public) :- ct(C).
    method_parameter(C-defcon,C-defcon-Name) :- ctf(C,_,_,Name).
        parameter_name(C-defcon-Name,Name) :- ctf(C,_,_,Name).
        parameter_type(C-defcon-Name,JType) :- ctf(C,_,_,Name,Type), type_javatype(Type,JType).
    method_statement(C-defcon,100,assign(this:Name,name(Name))) :- ctf(C,_,_,Name).

class_field(C,C-Name) :- ctf(C,_,_,Name).
    field_access(C-Name,public) :- ctf(C,_,_,Name).
    field_final(C-Name) :- ctf(C,_,_,Name).
    field_name(C-Name,Name) :- ctf(C,_,_,Name).
    field_type(C-Name,Type) :- ctf(C,_,_,Name,FT), type_javatype(FT,Type).

% ------- translate each web application state to Java field in the State class

class_name(state,'State').
class_packagename(state,P) :- get_package_name(app,P).
class_field(state,state-Name) :- state(Name).
    field_name(state-Name,Name) :- state(Name).
    field_type(state-Name,JT) :- state_type(Name,T), type_javatype(T,JT).
    field_access(state-Name,public) :- state(Name).
    field_initializer(state-Name,Init) :- state_initializer(Name,Init).

% ------- translate each web page to a Java method in the Pages class

class_name(pages,'Pages').
class_packagename(pages,P) :- get_package_name(app,P).
class_method(pages,pages-Page-Method) :- page_method(Page,Method).
    method_name(pages-Page-Method,Name) :- atomic_list_concat([Method,'_',Page],Name).
    method_returntype(pages-Page-Method,void) :- method_name(pages-Page-Method,_).
    method_access(pages-Page-Method,public) :- method_name(pages-Page-Method,_).

% ------- translate each route to a conditional in the Router class dispatch method

class_name(router,'Router').
class_packagename(router,P) :- get_package_name(app,P).
class_method(router,router-dispatch).
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
    ensure_app_dir,
    generate_class(_), fail.

generate.

generate_class(Class) :-
    class(Class),
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

    class_final(C,F) :- bool(class_final(C),F).

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
        bool(method_static(M),Static),
        bool(method_final(M),Final),
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
