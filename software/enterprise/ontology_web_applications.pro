:- module(ontology_web_applications,[
    webapp/1
    , webapp_page/2
    , webapp_state/2
    , state/1
    , state_type/2
    , state_initializer/2
    , page_method/2
    , page_path/2
]).
/** <module> Ontology for web applications

The computer should derive the web application from the specification.
*/

/** webapp(?AppId) is nondet.
    webapp_page(?AppId,?PageId) is nondet.
*/
:- multifile webapp/1,
             webapp_page/2,
             webapp_state/2.

/** page(?PageId) is nondet.
    page_method(?PageId,?Method) is nondet.
    page_path(?PageId,?Path) is nondet.
    page_content(?PageId,?Content) is nondet.

Method is an atom: =|get|= or =|post|=.
*/
:- multifile(state/1).
:- multifile(state_type/2).
:- multifile(state_initializer/2).
:- multifile(page/1).
:- multifile(page_method/2).
:- multifile(page_path/2).
:- multifile(page_content/2).
