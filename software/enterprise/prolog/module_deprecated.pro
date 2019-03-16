% -------------------- deprecated

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

% -------------------- loading

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

% ---------- deprecated

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

/** module_instantiates_ontology(?ModId,?OntId) is nondet.

A Prolog module may instantiate (be an instance of) an ontology.
*/
:- multifile module_instantiates_ontology/2.

prolog:message(loaded_ontology(Ont,Mod,File,N)) -->
    ['Loaded ontology ~w (~w predicates) into module ~w from file ~w with'-[Ont,N,Mod,File]].
prolog:message(instantiate_ontology(Ont,Mod)) --> ['Instantiating the ontology ~w as the module ~w'-[Ont,Mod]].
prolog:message(write_file(Path)) --> ['Writing file ~w'-[Path]].
prolog:message(generate_link(Host,Guest)) --> ['Generating link ~w :- ~w'-[Host,Guest]].
