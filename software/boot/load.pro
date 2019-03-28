:-  include("module.pro").
:-  consult("debug.pro").
:-  documentation:consult("doc.pro").

:-  current_prolog_flag(argv, Argv),
    forall(
        member(Arg, Argv),
        (
            absolute_file_name(Arg, Abs),
            consult(Abs)
        )
    ).
