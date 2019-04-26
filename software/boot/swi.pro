initialize_module(Module) :-
    Module:set_module(base(system)),
    Module:set_module(class(system)),
    delete_import_module(Module, user),
    add_import_module(Module, system, end).

assert_2(Module, Clause) :-
    '@'(system:assert(Clause), Module).

use_module_4(Importer, ExpRel, ExpAbs, ExpMod) :-
    '@'(system:use_module(ExpAbs), Importer),
    (source_file_property(ExpAbs, module(ExpMod))
    ->  true
    ;   throw_error(impossible(use_module(ExpRel),ExpAbs))).
