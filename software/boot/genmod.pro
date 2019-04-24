generate_module_name(AbsFilePath, Mod) :-
    gensym('genmod', Gensym),
    file_base_name(AbsFilePath, FileName),
    file_name_extension(Base, _, FileName),
    atomic_list_concat([Gensym,'_',Base], Mod).
