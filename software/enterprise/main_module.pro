% deprecated

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

/** path_prefix_extension(+Total,-Prefix,-Extension) is det.
    path_prefix_extension(-Total,+Prefix,+Extension) is det.

This is file_name_extension/3 but with less confusing name.
*/
path_prefix_extension(Total,Prefix,Extension) :-
    file_name_extension(Prefix,Extension,Total).
