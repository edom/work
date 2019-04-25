initialize_module(Module) :-
    Module:set_module(base(system)),
    Module:set_module(class(system)),
    delete_import_module(Module, user),
    add_import_module(Module, system, end).
