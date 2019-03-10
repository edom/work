:- module(java_program_from_web_app,[]).
:- use_module('./java_util.pro',[
    once_no_fail/1
    , javatype_javareftype/2
    , type_javaclassname/2
    , name_jname/2
    , stringies_concat/2
]).
/** <module> Translate a web application to a Java program

Translate from imported ontology_web_application.pro to ontology_java_program.pro.
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
             state/1,
             state_name/2,
             state_type/2,
             state_initializer/2,
             page_name/2,
             page_method/2,
             page_path/2.

% ------- wiring: presenting internal structure as ontology_java_program.pro structure

class(C) :- java_class(C,_,_,_).
class_constructor(C,K) :- java_class_constructor(C,K,_,_).
class_package_name(A,B) :- java_class(A,B,_,_).
class_name(A,B) :- java_class(A,_,B,_).
class_comment(C,K) :- java_class(C,_,_,L), member(comment-K,L).
class_extend(C,S) :- element_option(C, extend-A) -> S = some(A) ; S = none.
class_implements(C,L) :- element_option(C, implements-List) -> L = List ; L = [].
class_implement(C,I) :- class_implements(C,L), member(I,L).

class_field(A,B) :- java_class_field(A,B,_,_,_).
class_method(A,B) :- java_class_method(A,B,_,_,_,_).

field(A) :- java_class_field(_,A,_,_,_).
field_name(A,B) :- java_class_field(_,A,_,B,_).
field_type(A,B) :- java_class_field(_,A,B,_,_).
field_initializer(A,B) :-
    java_class_field(_,A,_,_,L),
    (member(initializer-I,L)
    ->  B = some(I)
    ;   B = none).

constructor(A) :- java_class_constructor(_,A,_,_).
method(A) :- java_class_method(_,A,_,_,_,_).
method_name(A,B) :- java_class_method(_,A,_,B,_,_).
method_return_type(A,B) :- java_class_method(_,A,B,_,_,_).

callable_parameter(Method,N,Method-N,Type,Name) :-
    java_class_method(_,Method,_,_,Params,_),
    nth1(I,Params,[Type,Name|_]),
    N is 100*I.

callable_parameter(Ctor,N,Ctor-N,Type,Name) :-
    java_class_constructor(_,Ctor,Params,_),
    nth1(I,Params,[Type,Name|_]),
    N is 100*I.

:- discontiguous callable_parameter/3,
                 parameter_name/2,
                 parameter_type/2.
callable_parameter(C,N,P) :- callable_parameter(C,N,P,_,_).
parameter_type(Param,Type) :- callable_parameter(_,_,Param,Type,_).
parameter_name(Param,Name) :- callable_parameter(_,_,Param,_,Name).

callable_options(Id,Opts) :- java_class_constructor(_,Id,_,Opts).
callable_options(Id,Opts) :- java_class_method(_,Id,_,_,_,Opts).

% callable_statement/3 is discontiguous so that we can insert aspects.
:- discontiguous callable_statement/3.
callable_statement(Callable,Order,Stmt) :-
    callable_options(Callable,Opts),
    member(body-Stmts,Opts),
    statements_(Order,Stmt,100,Stmts).

    % statements_(-Order,-Stmt,+BaseOrder,+List).
    % Helper for making choice points from a list.
    statements_(Order,Stmt,BaseOrder,List) :-
        nth0(N,List,Stmt),
        Order is BaseOrder + N.

callable_throws(Method,Throws) :-
    element_option(Method,throws-Throws) -> true ; Throws = [].

callable_throw(Method,Throw) :-
    callable_throws(Method,Throws),
    member(Throw,Throws).

element_access(A,public) :- element_option(A,public), !.
element_access(A,protected) :- element_option(A,protected), !.
element_access(A,package) :- element_option(A,package), !.
element_access(A,private) :- element_option(A,private), !.
element_access(_,public).

element_static(A,true) :- element_option(A,static), !.
element_static(_,false).

element_final(A,true) :- element_option(A,final), !.
element_final(_,false).

    element_options(A,B) :- java_class(A,_,_,B).
    element_options(A,B) :- java_class_constructor(_,A,_,B).
    element_options(A,B) :- java_class_method(_,A,_,_,_,B).
    element_options(A,B) :- java_class_field(_,A,_,_,B).

    element_option(A,B) :- element_options(A,L), member(B,L).

% ------- internal structure

/** java_class(?ClassId,?Package,?Name,?ClassOpts) is nondet.
    java_class_constructor(?ClassId,?CtorId,?Params,?CtorOpts) is nondet.
    java_class_field(?ClassId,?FieldId,?JavaType,?FieldName,?FieldOpts) is nondet.
    java_class_method(?ClassId,?MethodId,?ReturnType,?MethodName,?Params,?MethodOpts) is nondet.

Internal database inferred from rules.

Each of ClassOpts, FieldOpts, and MethodOpts may contain:
    - one of these: `public`, `protected`, `package`, `private`
    - one `static`
    - one `final`
    - many `comment`-C

Params is a list.
Each element of Params is again a list `[Type,Name|Opts]`.

MethodOpts may contain:
    - one `body`-L where L is a list of statements.

There are two ways to specify method body:
    - Specifying `body-L` option in MethodOpts
    - Asserting callable_statement/3

Do not mix those two ways.
*/
:- discontiguous java_class/4,
                 java_class_constructor/4,
                 java_class_field/5,
                 java_class_method/6.

% ------- package naming logic

compute_package(literal(A),Z) :- !, A = Z.
compute_package(default,Z) :- !, once_no_fail(default_package_name(Z)).
compute_package(program,Z) :- !, compute_package(default,Z).
compute_package(entity,Z) :- !, compute_package(program:literal(entity),Z).
compute_package(app,Z) :- !, compute_package(program:literal(app),Z).
compute_package(A:B,C) :- !,
    compute_package(A,A0),
    compute_package(B,B0),
    atomic_list_concat([A0,'.',B0],C).
compute_package(A,_) :- domain_error(package_expression,A).

% ------- map from types to Java types

type_javatype(T,J) :- type_integer_bit(T,N), 0 =< N, N =< 32, !, J = int.
type_javatype(T,J) :- type_integer_bit(T,N), 32 < N, N =< 64, !, J = long.
type_javatype(T,J) :- type_identifier_bit(T,N), 0 =< N, N =< 32, !, J = int.
type_javatype(T,J) :- type_identifier_bit(T,N), 32 < N, N =< 64, !, J = long.
type_javatype(T,J) :- type_string(T), !, J = 'java.lang.String'.
type_javatype(T,J) :- type_optional(T,A), !, type_javatype(A,P), javatype_javareftype(P,J).
type_javatype(T,_) :- throw(error(no_related_java_type_for_type(T),_)).

class_qualified_name(C,Q) :-
    class_package_name(C,P),
    class_name(C,N),
    atomic_list_concat([P,'.',N],Q).

java_class_type(C,T) :- class_qualified_name(C,T).

% ------- translate each record type to a Java entity class

% class-type
ct(recordtype-T, T) :- recordtype(T).
ct(C) :- ct(C,_).

% class-type-field
ctf(C,T,F,FN,FT) :- ct(C,T), recordtype_field(T,F,FN,FT).
ctf(C,T,F,FN) :- ctf(C,T,F,FN,_).
ctf(C,T,F) :- ctf(C,T,F,_).

java_class(C,Package,Name,[final,comment-(generated-from-recordtype-T)]) :-
    ct(C,T),
    compute_package(entity,Package),
    type_javaclassname(T,Name).

    java_class_constructor(C,C-defcon,[],[public]) :- ct(C).
        % XXX ordering
        callable_parameter(C-defcon,100,C-defcon-Name) :- ctf(C,_,_,Name).
            parameter_name(C-defcon-Name,Name) :- ctf(C,_,_,Name).
            parameter_type(C-defcon-Name,JType) :- ctf(C,_,_,Name,Type), type_javatype(Type,JType).
        callable_statement(C-defcon, 100, field(this,Name) := name(Name)) :- ctf(C,_,_,Name).

java_class_field(C,C-Name,JT,Name,[final]) :-
    ctf(C,_,_,Name,FT),
    type_javatype(FT,JT).

% ------- translate each web application state to Java field in the State class

java_class(state,Package,'State',[final]) :-
    compute_package(app,Package).

java_class_field(state,state-StateId,JT,JName,Init) :-
    state(StateId),
    once_no_fail(state_type(StateId,T)),
    once_no_fail(state_name(StateId,Name)),
    once_no_fail(state_initializer(StateId,OptInit)),
    name_jname(Name,JName),
    type_javatype(T,JT),
    fold_optional(OptInit, [], X->[initializer-X], Init).

    fold_optional(none, D, _, Z) :- !, D = Z.
    fold_optional(some(A), _, P->B, Z) :- !, A = P, B = Z.
    fold_optional(A,_,_,_) :- domain_error(optional,A).

% ------- generate main class

java_class(main,Package,'Main',[final]) :-
    compute_package(app,Package).

    java_class_method(main, main-main,
        void, main, [[array('java.lang.String'),args]],
        [public, static, body-Body, throws-['java.lang.Exception']]
    ) :-
        once_no_fail(java_class_type(state, TState)),
        once_no_fail(java_class_type(pages, TPages)),
        once_no_fail(java_class_type(router, TRouter)),
        Body = [
            % https://www.eclipse.org/jetty/documentation/9.4.x/embedded-examples.html
            let(_, state, new(TState))
            , let(_, pages, new(TPages,[$state]))
            , let(_, router, new(TRouter,[$pages]))
            , let(_, server, new('org.eclipse.jetty.server.Server',[8080]))
            , let(_, handler, new('org.eclipse.jetty.servlet.ServletHandler',[]))
            , let(_, holder, new('org.eclipse.jetty.servlet.ServletHolder',[$router]))
            , $server:setHandler($handler)
            , $handler:addServletWithMapping($holder,"/*")
            , $server:start
            , $server:join
        ].

% ------- translate each web page to a Java method in the Pages class

java_class(pages,Package,'Pages',[final]) :-
    compute_package(app,Package).

java_class_field(pages, pages-state, TState, state, [final]) :-
    once_no_fail(java_class_type(state, TState)).

java_class_constructor(pages, pages-ctor, [[TState,state]], [body-[
    field(this,state) := $state
]]) :-
    once_no_fail(java_class_type(state, TState)).

java_class_method(pages, pages-PageId-HttpMethod, void, JName, [], []) :-
    once_no_fail(page_method(PageId,HttpMethod)),
    once_no_fail(page_name(PageId,PageName)),
    stringies_concat([HttpMethod,'_',PageName],SName),
    name_jname(SName,JName).

% ------- translate each route to a conditional in the Router class service (dispatch) method

java_class(router, Package, 'Router', [final,extend-'javax.servlet.http.HttpServlet']) :-
    compute_package(app,Package).

java_class_field(router, router-pages, TPages, pages, [final]) :-
    once_no_fail(java_class_type(pages,TPages)).

java_class_constructor(router, router-ctor, [[TPages,pages]], [body-[
    field(this,pages) := $pages
]]) :-
    once_no_fail(java_class_type(pages,TPages)).

java_class_method(router, router-service,
    void, service, [
        ['javax.servlet.http.HttpServletRequest',request]
        , ['javax.servlet.http.HttpServletResponse',response]
    ],
    [protected, throws-['java.io.IOException', 'javax.servlet.ServletException']]
).

    callable_statement(router-service,Order,Stmt) :-
        statements_(Order,Stmt,100,[
            let('java.lang.String', pathInfo, $request:getPathInfo)
            , let('java.lang.String', method, $request:getMethod)
            , if($pathInfo == null, return)
        ]).

    callable_statement(router-service,200,
        if((SPath:equals($pathInfo)) /\ (SMethod:equals($method)), return)
    ) :-
        page_path(Page,Path),
        page_method(Page,HttpMethod),
        atom_string(HttpMethod,SMethod),
        atom_string(Path,SPath).

    callable_statement(router-service,300,
        $response:sendError($('javax.servlet.http.HttpServletResponse.SC_NOT_FOUND'))
    ).
