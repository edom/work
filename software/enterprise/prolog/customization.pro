:- module(prolog_customization,[
]).

/** <module> Tailoring Prolog to our requirements

*/

:- reexport('./ontology.pro',[
    directive/1
    , declare_class/2
]).
:- reexport('./component.pro',[
    connect_plug_to_socket/2
]).
:- reexport('./module.pro',[
    module_host/2
]).
