:- module(prolog_module,[
    % facts
    module/1,
    module_file/2,
    module_import/2,
    module_export/2,
    module_host/2,
    module_guest/2,
    module_instantiates_ontology/2,
    ontology/1,
    ontology_file/2,
    ontology_module/2,
    ontology_predicate/2,
    % actions
    load_ontologies/0,
    instantiate_ontologies/0,
    load_modules/0,
    link_modules/0,
    generate_link/3
]).
/** <module> Tailoring SWI-Prolog module system to our requirements

# Usage

    - Declare each ontology with ontology/1 and the ontology property predicates.
    - Declare each module with module/1 and the module property predicates.
    - Declare which module instantiates which ontology with module_instantiates_ontology/2.
    - Declare linking.
    - Call load_ontologies/0.
    - Call instantiate_ontologies/0 to dynamically declare multifile predicates
    according to module_instantiates_ontology/2.
    - Call load_modules/0.
    - Call link_modules/0.

# Design

A _module_ is a namespace that contains predicates.

Practices for a clash-free module system:
    - A file must not name its own module.
    - A file must not contain any call with explicit module (phrase of the form M:F).
    If file U depends on M:F, then U must declare a multifile predicate F, and let the linker link U:F and M:F.

There are several ways module A can import module B:
    - Import every exported predicate. This assumes no name clash.
    - Explicit linking.

By "linking", we mean asserting a clause for a multifile predicate.
*/

/** module(?ModuleId) is nondet.
    module_file(?ModuleId,?File) is nondet.
    module_import(?ModuleId,?Import) is nondet.
    module_export(?ModuleId,?Export) is nondet.

ModuleId is an atom.
It must be unique among all modules.

Import is an atom that identifies another module.

Export is a Name/Arity term.
*/
:- multifile
    module/1,
    module_file/2,
    module_import/2,
    module_export/2.

/** module_guest(?HostModuleId,?GuestModuleId) is nondet.
    module_guest(?HostModuleId,?OwnerHead,?GuestModuleId,?GuestHead) is nondet.

For linking multifile predicates.

==
Host:Pred(Arg) :- Guest:Pred(Arg).
==

For generate_link_file/1 and generate_link/3.

Example:

==
% a:P :- b:P.
module_guest(a,b).

% a:p(A) :- b:q(A).
module_guest(a,p(A),b,q(A)).
==
*/
:- multifile(module_guest/2).

/** ontology(?OntId) is nondet.
    ontology_file(?OntId,?FilePath) is nondet.
    ontology_module(?OntId,?ModId) is nondet.
    ontology_predicate(?OntId,?NameArity) is nondet.

ontology_predicate/2 defines a multifile/1 predicate.
It is affected by load_ontologies/0.
*/
:- multifile
    ontology/1,
    ontology_file/2,
    ontology_module/2,
    ontology_predicate/2.

ontology_predicate(Ont,NameArity) :-
    ontology_module(Ont,Module),
    module_host(Module,NameArity).

/** module_host(?ModuleId,?NameArity) is nondet.

NameArity describes a multifile predicate originally defined in module ModuleId.

Mnemonic: A host accepts guests.
See also module_guest/2.

For each multifile predicate P defined in HostId:
If GuestId:P is true, then HostId:P should be true.
That is, the knowledge base should behave as if this clause exists:

==
HostId:P :- GuestId:P.
==
*/
module_host(Module,Name/Arity) :-
    Pred = Module:Functor,
    predicate_property(Pred,multifile),
    predicate_property(Pred,implementation_module(Module)),
    functor(Functor,Name,Arity).

/** module_instantiates_ontology(?ModId,?OntId) is nondet.

A Prolog module may instantiate (be an instance of) an ontology.
*/
:- multifile module_instantiates_ontology/2.



% ------- procedural

/** load_ontologies

Load the modules defined by ontology_file/2 and ontology_module/2.
*/
load_ontologies :-
    foreach(ontology(Ont), load_ontology(Ont)).

load_ontology(Ont) :-
    ontology_module(Ont,Mod),
    ontology_file(Ont,File),
    Mod:load_files(File,[module(Mod)]),
    ontology_predicatecount(Ont,N),
    print_message(informational, loaded_ontology(Ont,Mod,File,N)).

    ontology_predicatecount(Ont,N) :-
        findall(P, ontology_predicate(Ont,P), Ps),
        length(Ps,N).

instantiate_ontologies :-
    foreach(module_instantiates_ontology(Mod,Ont), instantiate_ontology(Ont,Mod)).

    instantiate_ontology(Ont,InsMod) :-
        print_message(informational, instantiate_ontology(Ont,InsMod)),
        foreach(ontology_predicate(Ont,Pred), InsMod:multifile(Pred)).

/** load_modules

Load the modules defined by module/2.

Generate links according to module_guest/2.

Known problem: No dependency analysis.
The order of clauses of module/1 is the order in which the modules are loaded.
If A imports B, then you must make sure that module/1 produces B before A.
Clause ordering matters.
*/
load_modules :-
    foreach(module(Module), load_module(Module)),
    File = './generated_link.pro',
    generate_link_file(File),
    print_message(informational,consult(File)),
    consult(File).

    load_module(Module) :-
        module_file(Module,File),
        print_message(informational,loading_module(Module,File)),
        foreach(module_import(Module,Import),do_module_import(Module,Import)),
        Module:load_files(File,[module(Module)]),
        foreach(module_export(Module,Export),Module:export(Export)).

        do_module_import(Module,Import) :- functor(Import,_,0), !, add_import_module(Module,Import,end).
        do_module_import(_,Import) :- domain_error(module_import,Import).

link_modules :-
    false.%TODO

generate_link_file(File) :-
    print_message(informational,write_file(File)),
    setup_call_cleanup(
        open(File,write,Stream,[type(binary)]),
        foreach(module_guest(Host,Guest), generate_link(Host,Guest,Stream)),
        close(Stream)
    ).

/** generate_link(++HostModuleId,++GuestModuleId,++Stream) is semidet.

*/
generate_link(Host,Guest,Stream) :-
    foreach(generate_link0(Host,Guest,Stream), true).

    generate_link0(Host,Guest,Stream) :-
        print_message(informational,generate_link(Host,Guest)),
        Opts = [quoted(true),fullstop(true),nl(true),numbervars(true)],
        module_host(Host,Name/Arity),
        functor(Functor,Name,Arity),
        predicate_property(Guest:Functor,defined),
        numbervars(Functor,0,_),
        write_term(Stream,(Host:Functor :- Guest:Functor),Opts).

prolog:message(loaded_ontology(Ont,Mod,File,N)) -->
    ['Loaded ontology ~w (~w predicates) into module ~w from file ~w with'-[Ont,N,Mod,File]].
prolog:message(instantiate_ontology(Ont,Mod)) --> ['Instantiating the ontology ~w as the module ~w'-[Ont,Mod]].
prolog:message(loading_module(Module,File)) --> ['Loading module ~w from file ~w'-[Module,File]].
prolog:message(consult(Path)) --> ['Consulting file ~w'-[Path]].
prolog:message(write_file(Path)) --> ['Writing file ~w'-[Path]].
prolog:message(generate_link(Host,Guest)) --> ['Generating link ~w :- ~w'-[Host,Guest]].
