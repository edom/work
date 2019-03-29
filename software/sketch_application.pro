:- consult("sketch_application_swipl.pro").

% -------------------- structure

class(person).
class_property(person, name).
class_property(person, food).
class_property(person, birth_date).

% dummy test data
database(person, -2, [name-"Alice", food-"bread", birth_date-date(1990,1,2)]).
database(person, -1, [name-"Bob", food-"meat", birth_date-date(1990,3,4)]).

globalization_locale(eng).
globalization_locale(ind).
globalization_locale(jpn).

term_locale_string(class(person), eng, "person").
term_locale_string(class(person), ind, "orang").
term_locale_string(class(person), jpn, "人物").
term_locale_string(property(person,name), eng, "name").
term_locale_string(property(person,food), eng, "food").
term_locale_string(property(person,birth_date), eng, "birth date").
term_locale_string(property(person,name), ind, "nama").
term_locale_string(property(person,food), ind, "makanan").
term_locale_string(property(person,birth_date), ind, "tanggal lahir").
term_locale_string(property(person,name), jpn, "名前").
term_locale_string(property(person,food), jpn, "食物").
term_locale_string(property(person,birth_date), jpn, "誕生日").

% -------------------- test

run :-
    read_class(person, A),
    database_insert(person, A).

urlpath_filepath_type('/static/style.css', 'sketch_application.css', 'text/css;charset=UTF-8').
