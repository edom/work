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

There are three ways to specify method body (do not mix):
    - Recommended: Using the convenience predicate callable_statements/3
    - Specifying `body-L` option in MethodOpts
    - Asserting callable_statement/3 directly

*/
:- multifile java_class/4,
             java_class_constructor/4,
             java_class_field/5,
             java_class_method/6.

% ------- package naming logic

compute_package(literal(A),Z) :- !, A = Z.
compute_package(default,Z) :- !, once_no_fail(base_package_name(Z)).
compute_package(program,Z) :- !, compute_package(default,Z).
compute_package(entity,Z) :- !, compute_package(program:literal(entity),Z).
compute_package(app,Z) :- !, compute_package(program:literal(app),Z).
compute_package(A:B,C) :- !,
    compute_package(A,A0),
    compute_package(B,B0),
    atomic_list_concat([A0,'.',B0],C).
compute_package(A,_) :- domain_error(package_expression,A).

% ------- translate each record type to a Java entity class

% class-type
ct(recordtype-T, T) :- recordtype(T).
ct(C) :- ct(C,_).

% class-type-field
ctf(C,T,F,FN,FT) :- ct(C,T), recordtype_field(T,F,FN,FT).
ctf(C,T,F,FN) :- ctf(C,T,F,FN,_).
ctf(C,T,F) :- ctf(C,T,F,_).

java_class(entity-C,Package,Name,[final,comment-(generated-from-recordtype-T)]) :-
    ct(C,T),
    compute_package(entity,Package),
    type_javaclassname(T,Name).

    java_class_constructor(entity-C,entity-C-ctor,[],[public]) :- ct(C).
        % XXX ordering
        callable_parameter(entity-C-ctor,100,entity-C-ctor-param-Name,JType,Name,[]) :-
            ctf(C,_,_,Name,Type), type_javatype(Type,JType).
        callable_statement(entity-C-ctor, 100, field(this,Name) := name(Name)) :- ctf(C,_,_,Name).

java_class_field(entity-C,entity-C-field-Name,JT,Name,[final]) :-
    ctf(C,_,_,Name,FT),
    type_javatype(FT,JT).

% ------- State class generation

% Each web application state begets a Java field in the State class.

java_class(state,Package,'State',[final]) :-
    compute_package(app,Package).

java_class_field(state,state-StateId,JT,JName,Opts) :-
    state(StateId),
    once_no_fail(state_type(StateId,T)),
    once_no_fail(state_name(StateId,Name)),
    once_no_fail(state_initializer(StateId,Init)),
    name_jname(Name,JName),
    type_javatype(T,JT),
    Opts = [initializer-Init].

% Each database begets a DataSource field in the State class.

java_class_field(state,state-D,'javax.sql.DataSource',FieldName,[final]) :-
    database_name(D,N),
    atomic_list_concat([database_,N],FieldName).

java_class_constructor(state,state-ctor,[],[]).
    callable_parameter(state-ctor, 100, state-ctor-database-D,
        'javax.sql.DataSource', ParamName, []
    ) :-
        java_database_field_name(D,ParamName).
    callable_statement(state-ctor,100,field(this,F) := $F) :-
        java_database_field_name(_,F).

java_database_field_name(D,FieldName) :-
    database_name(D,Name),
    atomic_list_concat([database_,Name],FieldName).

% ------- main class generation

java_class(main,Package,'Main',[final]) :-
    compute_package(app,Package).

    java_class_method(main, main-main,
        void, main, [[array('java.lang.String'),args]],
        [public, static, throws-['java.lang.Exception']]
    ).
        % Logger.
        callable_statements(main-main, 100, [
            let(_, logger, cast('ch.qos.logback.classic.Logger',
                $'org.slf4j.LoggerFactory':getLogger($'org.slf4j.Logger.ROOT_LOGGER_NAME')
            ))
            , $logger:setLevel($'ch.qos.logback.classic.Level.INFO')
        ]).
        % Databases.
        callable_statements(main-main, 200, [
            let(_, Var, new('com.zaxxer.hikari.HikariDataSource'))
            , $Var:setJdbcUrl(JdbcUrl)
            , $Var:setUsername(Username)
            , $Var:setPassword(Password)
        ]) :-
            java_database_field_name(D,Var),
            once_no_fail(database_host(D,Host)),
            once_no_fail(database_port(D,Port)),
            once_no_fail(database_catalog(D,Catalog)),
            once_no_fail(database_username(D,AUsername)),
            once_no_fail(database_password(D,APassword)),
            atom_string(AUsername,Username),
            atom_string(APassword,Password),
            stringies_concat(["jdbc:postgresql://",Host,":",Port,"/",Catalog], JdbcUrl).
        % Servlet.
        callable_statements(main-main, 300, [
            % https://www.eclipse.org/jetty/documentation/9.4.x/embedded-examples.html
            let(_, state, new(TState,StateParams))
            , let(_, pages, new(TPages,[$state]))
            , let(_, router, new(TRouter,[$pages]))
            , let(_, server, new('org.eclipse.jetty.server.Server',[8080]))
            , let(_, handler, new('org.eclipse.jetty.servlet.ServletHandler',[]))
            , let(_, holder, new('org.eclipse.jetty.servlet.ServletHolder',[$router]))
            , $server:setHandler($handler)
            , $handler:addServletWithMapping($holder,"/*")
            , $server:start
            , $server:join
        ]) :-
            findall($DbFieldName, java_database_field_name(_,DbFieldName), StateParams),
            once_no_fail(java_class_type(state, TState)),
            once_no_fail(java_class_type(pages, TPages)),
            once_no_fail(java_class_type(router, TRouter)).

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
