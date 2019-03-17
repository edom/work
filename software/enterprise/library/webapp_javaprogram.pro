:- module(webapp_javaprogram,[]).
:- use_module("internal/java_util.pro",[
    once_no_fail/1
    , javatype_javareftype/2
    , type_javaclassname/2
    , name_jname/2
    , stringies_concat/2
]).

/** <module> Translate a web application to a Java program

Translate from imported web_application.pro to java_program.pro.

---+ How similar things differ

Eichberg 1998 translates Prolog to Java.
We are trying to use Prolog to define a model that translates to a Java web application.

Bibliography:
    - 1998, Michael Eichberg, "Compiling Prolog to Idiomatic Java"
    http://drops.dagstuhl.de/opus/volltexte/2011/3176/pdf/19.pdf

*/

% -------------------- begin customization section of language-user parameters

/** base_package_name(?PackageName) is det.

PackageName is an atom.
*/

:- multifile base_package_name/1.

% -------------------- end customization section

:- include("internal/ontology.pro").
:- include("internal/wj_ontology.pro").
:- include("internal/export.pro").
:- include("internal/java_type.pro").
:- include("internal/mapping.pro").
