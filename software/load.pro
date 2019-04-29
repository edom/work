:- import(user,[
    expand_file_name/2
    , my_consult/1
]).

should_load_path(Path) :-
    file_base_name(Path, Name),
    Name \= 'load.pro'.

paths_to_load(Paths) :-
    expand_file_name('*.pro', List),
    findall(Path, (member(Path, List), should_load_path(Path)), Paths).

load_my_files :-
    paths_to_load(Paths),
    my_consult(Paths).

% :- load_my_files.
