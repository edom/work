:- module(ontology_java_programs_write,[]).
:- use_module('./ontology_java_programs.pro').
:- use_module('./ontology_java_programs_write_dcg.pro',[]).
:- use_module(library(sgml_write),[
    xml_write/3
]).
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

% ------- meta-predicates

once_no_fail(G) :- call(G), !.
once_no_fail(G) :- throw(error(should_not_fail(G),_)).

% ------- mapping file paths

parts_path([],"") :- !.
parts_path([A],Z) :- !, term_string(A,Z,[quoted(false)]).
parts_path([A|B],Z) :- !,
    term_string(A,A0,[quoted(false)]),
    parts_path(B,B0),
    string_concat(A0,"/",A1),
    string_concat(A1,B0,Z).


program_dir(webapp-Prog,Dir) :- get_output_dir(Out), parts_path([Out,webapp,Prog],Dir).
program_dir_java(Prog,Dir) :- program_dir(Prog,Base), parts_path([Base,src,main,java],Dir).
program_class_source_file_path(Prog,C,Path) :-
    program_dir_java(Prog,Dir),
    class_package_name(C,Package),
    class_name(C,ClassName),
    split_string(Package,".","",PackageParts),
    string_concat(ClassName,".java",FileName),
    append([[Dir],PackageParts,[FileName]],Parts),
    parts_path(Parts,Path).

program_pom_xml_file(P,X) :- program_dir(P,D), parts_path([D,'pom.xml'],X).

get_output_dir(A) :- once_no_fail(output_dir(A)).

is_dry_run :- once_no_fail(dry_run(A)), A = true.

ensure_dir(Dir) :-
    is_dry_run
    ->  true
    ;   make_directory_path(Dir).

with_output_to_file(Path,Opts,Goal) :-
    is_dry_run
    ->  call(Goal)
    ;   (
            file_directory_name(Path,Dir),
            ensure_dir(Dir),
            setup_call_cleanup(
                open(Path,write,Stream,Opts),
                with_output_to(Stream,Goal),
                close(Stream)
            )
        ).

generate :- foreach(program(P), generate_program(P)).

generate_program(P) :-
    generate_pom_xml(P),
    foreach(program_class(P,C), generate_class(P,C)).

% ------- generate pom.xml

% https://www.eclipse.org/jetty/documentation/9.4.x/maven-and-jetty.html

program_dependency_maven(_,'org.eclipse.jetty','jetty-server','9.4.9.v20180320').

program_dependency_maven(P,G,A,V,compile) :- program_dependency_maven(P,G,A,V).
program_dependency_maven(_,'javax.servlet','javax.servlet-api','3.1.0',provided).

program_pom_myelem(Program,
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
        )
        , dependencies(DepElems)
        , build(
            plugins(
                plugin(
                    groupId("org.apache.maven.plugins")
                    , artifactId("maven-compiler-plugin")
                    , version("3.7.0")
                    , configuration(
                        source("1.8")
                        , target("1.8")
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
    program_maven_coordinates(Program,G,A,V),
    findall(DepElem, program_dependency_myelem(Program,DepElem), DepElems).

    program_dependency_myelem(P,dependency(groupId(-G),artifactId(-A),version(-V),scope(-S))) :-
        program_dependency_maven(P,G,A,V,S).

generate_pom_xml(Program) :-
    program_pom_xml_file(Program,PomXmlFile),
    program_pom_myelem(Program,Pom),
    print_message(informational, writing_pom_xml(Program,PomXmlFile)),
    with_output_to_file(PomXmlFile, [type(text),encoding(utf8)], write_myxml(Pom)).

prolog:message(writing_pom_xml(Prog,File)) -->
    ["Writing pom.xml for program ~w into ~w"-[Prog,File]].

write_myxml(Doc) :-
    current_output(Stream),
    myelem_xml(Doc,Xml),
    xml_write(Stream,Xml,[header(true)]).

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

% ------- generate classes

generate_class(Prog,Class) :-
    must_be(ground,Prog),
    must_be(ground,Class),
    once_no_fail(element_access(Class,Access)),
    once_no_fail(element_final(Class,Final)),
    once_no_fail(class_package_name(Class,Package)),
    once_no_fail(class_name(Class,Name)),
    once_no_fail(program_class_source_file_path(Prog,Class,Source)),
    print_message(informational,generate_class(Class,Source)),
    % XXX Java source files are not in UTF-8.
    with_output_to_file(Source, [type(text),encoding(utf8)], write_class(Class,Access,Final,Package,Name)).

    write_class(Class,Access,Final,Package,Name) :-
        write_phrase(ontology_java_programs_write_dcg:class_begin(Package,Name,Access,Final)),
        foreach(class_field(Class,Field), write_field(Field)),
        foreach(class_constructor(Class,Ctor), write_constructor(Ctor)),
        foreach(class_method(Class,Method), write_method(Method)),
        write_phrase(ontology_java_programs_write_dcg:class_end).

    write_phrase(P) :-
        once(phrase(P,Codes)),
        string_codes(Str,Codes),
        write(Str).

    write_field(Field) :-
        once_no_fail(element_access(Field,Access)),
        once_no_fail(element_static(Field,Static)),
        once_no_fail(element_final(Field,Final)),
        once_no_fail(field_name(Field,Name)),
        once_no_fail(field_type(Field,Type)),
        once_no_fail(field_initializer(Field,Init)),
        write_phrase(ontology_java_programs_write_dcg:field(Name,Type,Access,Static,Final,Init)).

    write_constructor(K) :-
        class_constructor(C,K),
        once_no_fail(class_name(C,Name)),
        once_no_fail(element_access(K,Access)),
        write("    "),
        write_phrase(ontology_java_programs_write_dcg:access(Access)),
        format("~w ",[Name]),
        write_method_parameters(K), write(" "),
        write_method_body(K).

    write_method(M) :-
        once_no_fail(element_access(M,Access)),
        once_no_fail(element_static(M,Static)),
        once_no_fail(element_final(M,Final)),
        once_no_fail(method_name(M,Name)),
        once_no_fail(method_return_type(M,Ret)),
        write("    "),
        write_phrase(ontology_java_programs_write_dcg:access(Access)),
        write_phrase(ontology_java_programs_write_dcg:static(Static)),
        write_phrase(ontology_java_programs_write_dcg:final(Final)),
        format("~w ~w ",[Ret,Name]),
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
        foreach(method_statement(M,_,S), write_phrase(ontology_java_programs_write_dcg:statement(S))),
        format("    }\n").

prolog:message(generate_class(Id,Source)) -->
    ['Writing Java class ~w into ~w'-[Id,Source]].
