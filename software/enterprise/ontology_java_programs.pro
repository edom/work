:- module(ontology_java_programs,[]).
/** <module> Ontology for Java programs

An instance of this ontology describes Java programs.

A Java program is what can be run in one JVM process.

A Java program is one Maven artifact.

A Java program is a set of Java classes.
A Java class is a text file, not a JVM class.
*/

/** program(?ProgId) is nondet.
    program_class(?ProgId,?ClassId) is nondet.
    program_maven_coordinates(?ProgId,?GroupId,?ArtifactId,?Version) is nondet.
*/
:- multifile program/1,
             program_class/2,
             program_maven_coordinates/4.

/** access(?Access) is nondet.

Access is any of these atoms: =|public|=, =|protected|=, =|package|=, =|private|=.
*/
access(public).
access(protected).
access(package).
access(private).

/** element_linecomment(?ElemId,?LineCommentText) is nondet.
    element_annotation(?ElemId,?AnnId) is nondet.
    annotation(?AnnId) is nondet.
    annotation_type(?AnnId,?JavaType) is nondet.
    annotation_argument(?AnnId,?Order,?ParamName,?ArgValue) is nondet.
    method_isconstructor(?MethodId) is nondet.

TODO implement these, rewrite old code
*/

/** class(?ClassId) is nondet.
    class_access(?ClassId,?Access) is nondet.
    class_final(?ClassId,?Final) is nondet.
    class_packagename(?ClassId,?PackageName) is nondet.
    class_name(?ClassId,?ClassName) is nondet.
    class_constructor(?ClassId,?CtorId) is nondet.
    class_method(?ClassId,?MethodId) is nondet.
    class_field(?ClassId,?FieldId) is nondet.
    class_comment(?ClassId,?Comment) is nondet.

The first parameter of these relations is the primary key.

Access is as defined in access/1.

ClassName is the simple name of the class,
the name without the package name.

Final is either true or false.
The predicate class_final/2 has two argument to distinguish between
deciding to make a class non-final
and forgetting to decide a class's finality.
*/
:- multifile(class/1).
:- multifile(class_access/2).
:- multifile(class_final/2).
:- multifile(class_packagename/2).
:- multifile(class_name/2).
:- multifile(class_constructor/2).
:- multifile(class_method/2).
:- multifile(class_field/2).
:- multifile(class_comment/2).

/** field_access(?FieldId,?Access) is nondet.
    field_final(?FieldId) is nondet.
    field_name(?FieldId,?FieldName) is nondet.
    field_type(?FieldId,?JavaType) is nondet.
    field_initializer(?FieldId,?JavaExp) is nondet.
*/
:- multifile(field/1).
:- multifile(field_access/2).
:- multifile(field_final/1).
:- multifile(field_name/2).
:- multifile(field_type/2).
:- multifile(field_initializer/2).

/** method_name(?MethodId,?MethodName) is nondet.
    method_returntype(?MethodId,?JavaType) is nondet.
    method_parameter(?MethodId,?ParamId) is nondet.
    method_statement(?MethodId,?StmtId) is nondet.
    method_statement(?MethodId,?Order,?StmtAst) is nondet.

Order is a natural number.

method_statement/2 is deprecated.
*/

:- multifile(method_name/2).
:- multifile(method_returntype/2).
:- multifile(method_parameter/2).
:- multifile(method_statement/2).
:- multifile(method_static/1).
:- multifile(method_final/1).

/** parameter_name(?ParamId,?ParamName) is nondet.
    parameter_type(?ParamId,?JavaType) is nondet.

Method parameter.
*/
:- multifile(parameter_name/2).
:- multifile(parameter_type/2).

/** statement_mustprecede(?X,?Y) is nondet.

Deprecated. Use method_statement/3.

Whether statement X must precede statement Y.

This influences the order of statements in a method body.

This does not have to be transitive.

If statement_mustprecede(X,Y) is true, then statement_mustprecede(Y,X) must be false.
*/
:- multifile(statement_ast/2).
:- multifile(statement_mustprecede/2).

:- multifile method_body/2.
method_body(C,_) :- \+ground(C), instantiation_error(C).
method_body(C,B) :-
    % TODO Order the statements according to statement_mustprecede/2
    findall(Ast, (method_statement(C,Id), statement_ast(Id,Ast)), B).
