/** <module> decompiling MIPS instructions to abstract statements

## Usage

### What inputs the user has to provide

```
:- use_module('./ps1_decompile.pro').
```

Define a memory_file/3, for the data source of address_instruction/2.

Define some routine_begin/2, for basic block leader analysis.

### The expected workflow

Begin with a file such as 'ps1dec_vhnk.pro'.

You want to understand what a routine does.
    - You decompile/1 that routine.
    - You try to understand the routine from the decompilation result.
    - You record your understanding by adding a clause of routine_begin/2.
    The second argument is for your comment.

You want to understand what a memory location stores.

Other things:
    - disassemble_routine/1, disassemble/1, disassemble/2

(TODO Write a decompile/2 that doesn't use dynamic predicates.)
*/
:- module(ps1_decompile, [
    decompile/1
    , decompile/2
]).

:- use_module(library(clpfd)).
:- use_module(library(ugraphs)).
:- use_module('./map.pro').
:- use_module('./ps1_analysis_0.pro').
:- use_module('./ps1_disassemble.pro').
:- use_module('./ps1_cfg.pro').
:- use_module('./ps1_procedural.pro').

/** decompile(+Begin)

Decompile the routine beginning at address Begin.
*/
decompile(Begin) :-
    ensure_routine_begin(Begin, Comment),
    routine_end(Begin, End),
    print_message(informational, decompiling_routine(Begin, End, Comment)),
    decompile(Begin, End).

prolog:message(decompiling_routine(Begin, End, Comment)) :-
    ['Decompiling routine 0x~16r -- 0x~16r (~w):'-[Begin, End, Comment]].

/** decompile(+Begin, +End)

Decompile the routine from address Begin inclusive to address End exclusive.

See also range_blocks/2.
*/
decompile(Begin, End) :-
    ensure_routine_begin(Begin, _),
    range_blocks(Begin-End, Blocks),
    cfg_naive_from_blocks(Blocks),
    cfg_annex_dominated_basic_blocks,
    cfg_simplify,
    cfg_annex_dominated_basic_blocks,
    cfg_print.

/** ensure_routine_begin(+Begin, +Comment)

Begin is an integer.

This is important because routine_begin/2 affects basic-block leader detection.
*/
ensure_routine_begin(Begin, _) :- \+ integer(Begin), throw(invalid_argument(Begin)).
ensure_routine_begin(Begin, Comment) :- routine_begin(Begin, Comment), !.
ensure_routine_begin(Begin, '') :-
    assertz(routine_begin(Begin, '')),
    print_message(warning, assume_routine_begin(Begin)).

prolog:message(assume_routine_begin(Begin)) -->
    ['ps1_decompile: 0x~16r is not a known routine. Asserting routine_begin/2 because it is required for basic block leader analysis.'-[Begin]].
