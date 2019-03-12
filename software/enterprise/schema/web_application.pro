:- module(schema_web_application,[
    state/1
    , state_name/2
    , state_type/2
    , state_initializer/2
    , page/1
    , page_name/2
    , page_method/2
    , page_content/2
    , page_path/2
]).
/** <module> Schema for a web application
*/

/** state_name(?StateId,?Name) is nondet.
    state_initializer(?StateId,?Init) is nondet.

Name is for field/variable names.
*/
:- multifile state/1,
             state_name/2,
             state_type/2,
             state_initializer/2.

/** page(?PageId) is nondet.
    page_method(?PageId,?Method) is nondet.
    page_path(?PageId,?Path) is nondet.
    page_content(?PageId,?Content) is nondet.
    page_name(?PageId,?Name) is nondet.

Method is an atom that represents a HTTP method.
Note that HTTP methods are case-sensitive.

Name is a hint for code generators.
*/
:- multifile page/1,
             page_name/2,
             page_method/2,
             page_path/2,
             page_content/2.
