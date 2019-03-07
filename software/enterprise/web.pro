/** <module> Web application model

The computer should derive the web application from the specification.
*/

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
