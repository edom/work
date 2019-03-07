/** <module> Java program model

*/

/** class(?ClassId) is nondet.
    class_access(?ClassId,?Access) is nondet.
    class_final(?ClassId) is nondet.
    class_packagename(?ClassId,?PackageName) is nondet.
    class_name(?ClassId,?ClassName) is nondet.
    class_constructor(?ClassId,?CtorId) is nondet.
    class_method(?ClassId,?MethodId) is nondet.
    class_field(?ClassId,?FieldId) is nondet.
    class_comment(?ClassId,?Comment) is nondet.
    method_name(?MethodId,?MethodName) is nondet.
    method_returntype(?MethodId,?JavaType) is nondet.
    method_parameter(?MethodId,?ParamId) is nondet.
    method_statement(?MethodId,?StmtId) is nondet.
    method_statement(?MethodId,?Order,?Stmt) is nondet.
    parameter_name(?ParamId,?ParamName) is nondet.
    parameter_type(?ParamId,?JavaType) is nondet.
    field_access(?FieldId,?Access) is nondet.
    field_final(?FieldId) is nondet.
    field_name(?FieldId,?FieldName) is nondet.
    field_type(?FieldId,?JavaType) is nondet.
    field_initializer(?FieldId,?JavaExp) is nondet.

The first parameter of these relations is the primary key.

ClassName is the simple name of the class,
the name without the package name.

Access is any of these atoms: =|public|=, =|protected|=, =|package|=, =|private|=.
*/

:- multifile(class/1).
:- multifile(class_access/2).
:- multifile(class_final/1).
:- multifile(class_packagename/2).
:- multifile(class_name/2).
:- multifile(class_constructor/2).
:- multifile(class_method/2).
:- multifile(class_field/2).
:- multifile(class_comment/2).
:- multifile(method_name/2).
:- multifile(method_returntype/2).
:- multifile(method_parameter/2).
:- multifile(method_statement/2).
:- multifile(method_static/1).
:- multifile(method_final/1).
:- multifile(parameter_name/2).
:- multifile(parameter_type/2).
:- multifile(field/1).
:- multifile(field_access/2).
:- multifile(field_final/1).
:- multifile(field_name/2).
:- multifile(field_type/2).
:- multifile(field_initializer/2).
:- multifile(statement_ast/2).

/** statement_mustprecede(?X,?Y) is nondet.

Whether statement X must precede statement Y.

This influences the order of statements in a method body.

This does not have to be transitive.

If statement_mustprecede(X,Y) is true, then statement_mustprecede(Y,X) must be false.
*/
:- multifile(statement_mustprecede/2).

method_body(C,_) :- \+ground(C), instantiation_error(C).
method_body(C,B) :-
    % TODO Order the statements according to statement_mustprecede/2
    findall(Ast, (method_statement(C,Id), statement_ast(Id,Ast)), B).
