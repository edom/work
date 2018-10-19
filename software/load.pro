should_load_path(Path) :- true
    , file_base_name(Path, Name)
    , dif(Name, 'load.pro').

paths_to_load(Paths) :- true
    , expand_file_name('*.pro', List)
    , findall(Path, (member(Path, List), should_load_path(Path)), Paths)
    .

consult_my_files :- true
    , paths_to_load(Paths)
    , consult(Paths).

:- consult_my_files.
