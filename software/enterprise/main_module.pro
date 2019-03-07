% The language user should not have to change this.

% ------- modules defining the specification language

module(syntax,[
    file-'./syntax.pro',
    exports-[
        '#'/2
        , ':='/2
    ]
]).
module(spec,[
    file-'./spec.pro',
    imports-[syntax],
    exports-[
        type_definition/2
        , type_maxbitcount/2
        , type_maxbytecount/2
        , type_normalform/2
        , recordtype/1
        , recordtype_field/2
        , recordtype_fields/2
        , field_name/2
        , field_type/2
        , type_natural/1
        , type_integer/1
        , type_integer_bit/2
        , type_identifier/1
        , type_identifier_bit/2
        , type_string/1
        , type_optional/2
    ]
]).

% ------- modules translating specification to implementation

module(java,[
    file-'./java.pro',
    exports-[
        class/1
        , class_access/2
        , class_final/1
        , class_packagename/2
        , class_name/2
        , class_constructor/2
        , class_method/2
        , class_field/2
        , class_comment/2
        , method_name/2
        , method_returntype/2
        , method_parameter/2
        , method_statement/2
        , parameter_name/2
        , parameter_type/2
        , field/1
        , field_access/2
        , field_final/1
        , field_name/2
        , field_type/2
        , statement_ast/2
        , statement_mustprecede/2

        , method_body/2
    ]
]).
module(java_type,[
    file-'./java_type.pro',
    imports-[spec],
    exports-[
        type_javatype/2,
        typename_javaclassname/2
    ]
]).
module(translation_java_dcg,[
    file-'./translation_java_dcg.pro',
    imports-[java_type, java],
    exports-[
        class_begin//4
        , class_end//0
        , field//4
        , field//5
        , statement//1
        , expression//1
        , access//1
        , final//1
        , static//1
    ]
]).
module(translation_java,[
    file-'./translation_java.pro',
    imports-[java_type, java, translation_java_dcg, web],
    exports-[
        generate/0
    ]
]).
module(web,[
    file-'./web.pro',
    imports-[java_type, java]
]).

% ------- modules from specs

module(Module,[
    file-File,
    imports-[syntax]
]) :- specmodule_path(Module,File).

specmodule_path(Module,Path) :-
    specfile(Path),
    file_base_name(Path,Base),
    path_prefix_extension(Base,Prefix,_),
    atom_concat(spec_,Prefix,Module).

specmodule(Module) :- specmodule_path(Module,_).

% ------- linking of multifile predicates

module_guest(spec,Guest) :- specmodule(Guest).
module_guest(web,Guest) :- specmodule(Guest).
module_guest(web,spec).

% ------- loading of spec files

/** specfile(?Path) is nondet.

Find spec files in spec file_search_path/2.

Not recursive.

If you are using pro instead of pl extension,
you may have to add this prolog_file_type/2 clause in your swiplrc:

==
prolog_file_type(pro,prolog).
==
*/
specfile(Path) :-
    file_search_path(spec,Dir),
    directory_files(Dir,List),
    member(File,List),
    path_prefix_extension(File,_,Ext),
    prolog_file_type(Ext,prolog),
    atomic_list_concat([Dir,'/',File],Path).

% file_name_extension's confusing name is historical.
path_prefix_extension(Total,Prefix,Extension) :- file_name_extension(Prefix,Extension,Total).
