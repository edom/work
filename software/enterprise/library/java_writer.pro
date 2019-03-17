:- module(java_writer,[]).
:- use_module("internal/java_util.pro",[
    once_no_fail/1
    , parts_path/2
    , statement_normalize/2
    , expression_normalize/2
    , check_statement/1
    , check_expression/1
]).
:- use_module("internal/my_sgml_write.pro",[
    write_myxml/1
]).
:- use_module("internal/java_writer_dcg.pro",[]).
/** <module> Write known programs into file system
*/

% ------- language user parameters

/** output_dir(?OutputDir) is det.
    dry_run(?Boolean) is det.

OutputDir is a string without trailing slash.

If dry_run is true, then the generated files are
written to standard output instead of to the file system.
*/
:- multifile output_dir/1.
:- multifile dry_run/1.

% ------- end of language user parameters

:- multifile webapp_program/2.

get_output_dir(A) :- once_no_fail(output_dir(A)).

is_dry_run :- once_no_fail(dry_run(A)), A = true.

ensure_dir(Dir) :-
    is_dry_run
    ->  true
    ;   java_util:ensure_dir(Dir).

:- meta_predicate with_output_to_file(+,+,0).
with_output_to_file(Path,Opts,Goal) :-
    is_dry_run
    ->  call(Goal)
    ;   java_util:with_output_to_file(Path,Opts,Goal).

% ------- mapping file paths

class_source(C,Path) :-
    get_output_dir(Dir),
    class_package_name(C,Package),
    class_name(C,ClassName),
    split_string(Package,".","",PackageParts),
    string_concat(ClassName,".java",FileName),
    append([[Dir,src,main,java],PackageParts,[FileName]],Parts),
    parts_path(Parts,Path).

pom_xml_file(X) :- get_output_dir(D), parts_path([D,'pom.xml'],X).

generate :-
    generate_pom_xml,
    foreach(class(C), generate_class(C)).

% ------- generate pom.xml

% https://www.eclipse.org/jetty/documentation/9.4.x/maven-and-jetty.html

maven_dependency(G,A,V) :-
    JettyVersion = '9.4.9.v20180320',
    member(G-A-V,[
        % HTTP server
        'org.eclipse.jetty'-'jetty-server'-JettyVersion
        , 'org.eclipse.jetty'-'jetty-servlet'-JettyVersion
        , 'javax.servlet'-'javax.servlet-api'-'3.1.0'
        % JDBC
        , 'com.zaxxer'-'HikariCP'-'3.3.1'
        , 'org.postgresql'-'postgresql'-'42.2.5'
        % logging
        , 'ch.qos.logback'-'logback-classic'-'1.2.3'
    ]).

maven_dependency(G,A,V,compile) :- maven_dependency(G,A,V).

program_pom_myelem(
    project(
        xmlns="http://maven.apache.org/POM/4.0.0"
        , 'xmlns:xsi'="http://www.w3.org/2001/XMLSchema-instance"
        , 'xsi:schemaLocation'="http://maven.apache.org/POM/4.0.0 https://maven.apache.org/maven-v4_0_0.xsd"
        , modelVersion("4.0.0")
        , groupId(-G)
        , artifactId(-A)
        , version(-V)
        , properties(
            'project.build.sourceEncoding'("UTF-8")
            , 'java.version'("1.8")
        )
        , dependencies(DepElems)
        , build(
            plugins(
                plugin(
                    groupId("org.apache.maven.plugins")
                    , artifactId("maven-compiler-plugin")
                    , version("3.7.0")
                    , configuration(
                        source("${java.version}")
                        , target("${java.version}")
                    )
                )
            )
        )
        % Tell Maven to copy the transitive dependencies to the "target/dependency" directory.
        % This simplifies running the application (java -cp 'dependency/*:*').
        % We do not use fat JARs because we do not want to prevent rsync from doing its job.
        , profiles(
            profile(
                id("release")
                , build(
                    plugins(
                        plugin(
                            groupId("org.apache.maven.plugins")
                            , artifactId("maven-dependency-plugin")
                            , version("3.0.1")
                            , executions(
                                execution(
                                    phase("package")
                                    , goals(
                                        goal("copy-dependencies")
                                    )
                                    , configuration(
                                        prependGroupId("true")
                                        , includeScope("runtime")
                                        , overWriteSnapshots("true")
                                        , overWriteReleases("true")
                                        , outputDirectory("${project.build.directory}/dependency")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
) :-
    maven_coordinates(G,A,V),
    findall(DepElem, dependency_myelem(DepElem), DepElems).

    dependency_myelem(dependency(groupId(-G),artifactId(-A),version(-V),scope(-S))) :-
        maven_dependency(G,A,V,S).

generate_pom_xml :-
    pom_xml_file(PomXmlFile),
    program_pom_myelem(Pom),
    print_message(informational, writing_pom_xml(PomXmlFile)),
    with_output_to_file(PomXmlFile, [type(text),encoding(utf8)], write_myxml(Pom)).

prolog:message(writing_pom_xml(File)) -->
    ["Writing pom.xml into ~w"-[File]].

% ------- generate classes

generate_class(Class) :-
    must_be(ground,Class),
    once_no_fail(element_access(Class,Access)),
    once_no_fail(element_final(Class,Final)),
    once_no_fail(class_package_name(Class,Package)),
    once_no_fail(class_name(Class,Name)),
    once_no_fail(class_extend(Class,OptSuperClassType)),
    once_no_fail(class_implements(Class,IfaceTypeList)),
    once_no_fail(class_source(Class,Source)),
    print_message(informational,generate_class(Class,Source)),
    % XXX Java source files are not in UTF-8.
    with_output_to_file(Source, [type(text),encoding(utf8)],
        write_class(Class,Access,Final,Package,Name,OptSuperClassType,IfaceTypeList)).

    prolog:message(generate_class(Id,Source)) -->
        ['Writing Java class ~w into ~w'-[Id,Source]].

% ------- writing

write_java_dcg(Rule) :-
    once_no_fail(java_writer_dcg:write_java_dcg(Rule)).

write_class(Class,Access,Final,Package,Name,OptSuperClassType,IfaceTypeList) :-
    write_java_dcg(class_begin(Package,Name,Access,Final,OptSuperClassType,IfaceTypeList)),
    foreach(class_field(Class,Field), write_field(Field)),
    foreach(class_constructor(Class,Ctor), write_executable(Ctor)),
    foreach(class_method(Class,Method), write_executable(Method)),
    write_java_dcg(class_end).

write_field(Field) :-
    once_no_fail(element_access(Field,Access)),
    once_no_fail(element_static(Field,Static)),
    once_no_fail(element_final(Field,Final)),
    once_no_fail(field_name(Field,Name)),
    once_no_fail(field_type(Field,Type)),
    once_no_fail(field_initializer_n(Field,Init)),
    write_java_dcg(field(Name,Type,Access,Static,Final,Init)).

    field_initializer_n(Field,none) :- field_initializer(Field,none), !.
    field_initializer_n(Field,some(Init)) :- field_initializer(Field,some(E)), !,
        expression_normalize(E,Init),
        check_expression(Init).

write_executable(E) :-
    once_no_fail(element_access(E,Access)),
    once_no_fail(element_static(E,Static)),
    once_no_fail(element_final(E,Final)),
    once_no_fail(callable_throws(E,Throws)),
    write("    "),
    write_java_dcg(access(Access)),
    write_java_dcg(static(Static)),
    write_java_dcg(final(Final)),
    % return type
    (method(E)
    ->  once_no_fail(method_return_type(E,Ret)), write_type(Ret), write(" ")
    ;   true
    ),
    % name
    (constructor(E)
    ->  once_no_fail(class_constructor(C,E)), once_no_fail(class_name(C,Name))
    ;   once_no_fail(method_name(E,Name))
    ),
    write(Name), write(" "),
    write_callable_parameters(E),
    write_java_dcg(throws(Throws)),
    write(" "),
    write_callable_body(E).

write_callable_parameters(E) :-
    write("("),
    % TODO order
    findall(P, callable_parameter(E,_,P), Ps),
    write_parameters(Ps),
    write(")").

    write_parameters([]) :- !.
    write_parameters([A]) :- !, write_parameter(A).
    write_parameters(A) :- !, write_parameters_many(A).

    write_parameters_many([]) :- !.
    write_parameters_many([A]) :- !, write("\n        "), write_parameter(A), write("\n    ").
    write_parameters_many([A|B]) :- !, write("\n        "),
        write_parameter(A), write(","), write_parameters_many(B).

        write_parameter(P) :-
            once_no_fail(parameter_name(P,Name)),
            once_no_fail(parameter_type(P,Type)),
            write_type(Type), write(" "),
            write_name(Name).

write_type(Type) :-
    write_java_dcg(type(Type)).

write_name(Name) :-
    write_java_dcg(name(Name)).

write_callable_body(E) :-
    write("{\n"),
    % TODO order before printing
    foreach(callable_statement(E,_,S), write_statement(S)),
    write("    }\n").

write_statement(S) :-
    once_no_fail(statement_normalize(S,S0)),
    once_no_fail(check_statement(S0)),
    write_java_dcg(statement(S0)).
