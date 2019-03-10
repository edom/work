:- module(ontology_java_program,[
    maven_coordinates/3

    , element_access/2
    , element_final/2
    , element_static/2
    , element_annotation/3

    , class/1
    , class_extend/2
    , class_implement/2
    , class_implements/2
    , class_constructor/2
    , class_field/2
    , class_method/2
    , class_name/2
    , class_package_name/2

    , field_initializer/2
    , field_name/2
    , field_type/2

    , callable_parameter/3
    , callable_statement/3
    , callable_throw/2
    , callable_throws/2
    , constructor/1
    , method/1
    , method_name/2
    , method_return_type/2

    , parameter_name/2
    , parameter_type/2

    , check_ontology/0
]).
/** <module> Ontology for a Java program

An instance of this ontology describes a Java program.

A Java program is what can be run in one JVM process.

A Java program is one Maven artifact.

A Java program is a set of Java classes.
A Java class is a text file, not a JVM class.
*/

/** maven_coordinates(?GroupId,?ArtifactId,?Version) is nondet.

Version is an atom.
Version should have three components.
Each component should be a number.
Example Version: '1.23.456'.
*/
:- multifile maven_coordinates/3.

/** access(?Access) is nondet.

Access is any of these atoms: =|public|=, =|protected|=, =|package|=, =|private|=.
*/
access(public).
access(protected).
access(package).
access(private).

/** element_access(+ElemId,-Access) is semidet.
    element_static(+ElemId,-Static) is semidet.
    element_final(+ElemId,-Final) is semidet.
    element_annotation(?ElemId,?AnnType,?AnnArgs) is nondet.

Access is as defined in access/1.

Final is either true or false.
The predicate class_final/2 has two argument to distinguish between
deciding to make an element non-final
and forgetting to decide an element's finality.

Static is either true or false.
*/
:- multifile element_access/2,
             element_final/2,
             element_static/2,
             element_annotation/3.

/** class(?ClassId) is nondet.
    class_extend(?ClassId,?OptSuperClassType) is nondet.
    class_implement(?ClassId,?IfaceType) is nondet.
    class_implements(?ClassId,?IfaceTypeList) is nondet.
    class_package_name(?ClassId,?PackageName) is nondet.
    class_name(?ClassId,?ClassName) is nondet.
    class_constructor(?ClassId,?CtorId) is nondet.
    class_method(?ClassId,?MethodId) is nondet.
    class_field(?ClassId,?FieldId) is nondet.
    class_comment(?ClassId,?Comment) is nondet.

The first parameter of these relations is the primary key.

ClassName is the simple name of the class,
the name without the package name.
*/
:- multifile class/1,
             class_extend/2,
             class_implement/2,
             class_implements/2.
:- multifile(class_package_name/2).
:- multifile(class_name/2).
:- multifile(class_constructor/2).
:- multifile(class_method/2).
:- multifile(class_field/2).
:- multifile(class_comment/2).

/** field_access(?FieldId,?Access) is nondet.
    field_final(?FieldId,?Boolean) is nondet.
    field_name(?FieldId,?FieldName) is nondet.
    field_type(?FieldId,?JavaType) is nondet.
    field_initializer(+FieldId,-OptJavaExp) is semidet.
*/
:- multifile(field/1).
:- multifile(field_name/2).
:- multifile(field_type/2).
:- multifile(field_initializer/2).

/** method_name(?MethodId,?MethodName) is nondet.
    method_return_type(?MethodId,?JavaType) is nondet.
    callable_parameter(?MethodId,?Order,?ParamId) is nondet.
    callable_statement(?MethodId,?Order,?StmtAst) is nondet.
    callable_throw(?MethodId,?Throwable) is nondet.

Order is a natural number.

Throwable is an atom representing a Java type.

A constructor is not a method, but each of them is a _callable_.
*/

:- multifile constructor/1,
             method/1,
             method_name/2,
             method_return_type/2,
             callable_parameter/3,
             callable_statement/3,
             callable_throw/2,
             callable_throws/2.

/** parameter_name(?ParamId,?ParamName) is nondet.
    parameter_type(?ParamId,?JavaType) is nondet.

Method parameter.
*/
:- multifile(parameter_name/2).
:- multifile(parameter_type/2).

% ------- some checking

element(A) :- class(A).
element(A) :- method(A).
element(A) :- field(A).

% We can refactor this check into one line
% if we use an RDF-triple-like predicate for everything.
error(missing_property(element,C,access)) :- element(C), \+element_access(C,_).
error(missing_property(element,C,final)) :- element(C), \+element_final(C,_).
error(missing_property(element,C,static)) :- element(C), \+element_static(C,_).
error(missing_property(class,C,name)) :- class(C), \+class_name(C,_).
error(missing_property(class,C,package_name)) :- class(C), \+class_package_name(C,_).
error(missing_property(field,F,name)) :- field(F), \+field_name(F,_).
error(missing_property(field,F,type)) :- field(F), \+field_type(F,_).
error(missing_property(method,M,name)) :- method(M), \+method_name(M,_).
error(missing_property(method,M,return_type)) :- method(M), \+method_return_type(M,_).

check_ontology :- \+error(_), !.
check_ontology :- error(E), print_message(error,E), fail.

prolog:error_message(missing_property(Class,Individual,PropName)) -->
    ["The ~w ~w is missing property ~w"-[Class,Individual,PropName]].
