:- include("enterprise/syntax.pro").
:- import(file("sketch_application_swipl.pro"),[
    start_http_server/0
    , check_web_app/0
    , multifiles([
        class/1
        , class_property/2
        , class_property_type/3
        , globalization_locale/1
        , term_locale_string/3
        , urlpath_filepath_type/3
        , database/3
    ])
]).

% -------------------- structure

:- discontiguous
    class/1,
    class_property/2,
    class_property_type/3.

class_property(A,B) :- class_property_type(A,B,_).

class(person).
class_property_type(person, name, #string(64)).
class_property_type(person, food, #string(32)).
class_property_type(person, birth_date, #string(16)).

class(employee).
class_property_type(employee, name, #string(64)).

class(department).
class_property_type(department, name, #string(64)).

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
