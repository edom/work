:- module(modules,[
    % facts
    module/1,
    module_file/2,
    module_import/2,
    module_export/2,
    module_guest/2,
    % actions
    load_modules/0,
    generate_link/3
]).

/** <module> Tailoring SWI-Prolog module system to our requirements

A file must not name its own module.

A file must not contain any call with explicit module (phrase of the form M:F).

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
    module/1
    , module_file/2
    , module_import/2
    , module_export/2
    .

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
        print_message(informational,load(Module,File)),
        foreach(module_import(Module,Import),do_module_import(Module,Import)),
        Module:consult(File),
        foreach(module_export(Module,Export),Module:export(Export)).

        do_module_import(Module,Import) :- functor(Import,_,0), !, add_import_module(Module,Import,end).
        do_module_import(_,Import) :- domain_error(module_import,Import).

generate_link_file(File) :-
    print_message(informational,write_file(File)),
    setup_call_cleanup(
        open(File,write,Stream,[type(binary)]),
        foreach(module_guest(Host,Guest), generate_link(Host,Guest,Stream)),
        close(Stream)
    ).

/** module_host(?ModuleId,?NameArity) is nondet.

NameArity describes a multifile predicate originally defined in module ModuleId.

Mnemonic: A host accepts guests.
See also module_guest/2.
*/
module_host(Module,Name/Arity) :-
    Pred = Module:Functor,
    predicate_property(Pred,multifile),
    predicate_property(Pred,implementation_module(Module)),
    functor(Functor,Name,Arity).

/** generate_link(++HostModuleId,++GuestModuleId,++Stream) is semidet.

*/
generate_link(Host,Guest,Stream) :-
    foreach(generate_link0(Host,Guest,Stream),true).

    generate_link0(Host,Guest,Stream) :-
        print_message(informational,generate_link(Host,Guest)),
        Opts = [quoted(true),fullstop(true),nl(true),numbervars(true)],
        module_host(Host,Name/Arity),
        functor(Functor,Name,Arity),
        predicate_property(Guest:Functor,defined),
        numbervars(Functor,0,_),
        write_term(Stream,(Host:Functor :- Guest:Functor),Opts).

prolog:message(load(Module,File)) --> ['Loading module ~w from file ~w'-[Module,File]].
prolog:message(consult(Path)) --> ['Consulting file ~w'-[Path]].
prolog:message(write_file(Path)) --> ['Writing file ~w'-[Path]].
prolog:message(generate_link(Host,Guest)) --> ['Generating link ~w :- ~w'-[Host,Guest]].
