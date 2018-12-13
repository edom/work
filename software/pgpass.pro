:- module(pgpass, [
    file//1
]).
/** <module> PostgreSQL pgpass file

The pgpass file format is documented at https://www.postgresql.org/docs/9.3/libpq-pgpass.html

We can't use library(csv) to parse a pgpass file because they have different escaping rules.

    - Use phrase/2 or phrase_from_file/2.
    - Use expand_file_name/2 to expand environment variables.
    - Use once/1 to cut choice points.

Example:
```
?- expand_file_name("$HOME/.pgpass", [Path|_]), once(phrase_from_file(file(Recs), Path)).
```
*/

/** file(?Records)//

A PostgreSQL pgpass file.

Records is a list.
Each of Records is one of these:
    - record(Host,Port,Catalog,User,Pass) where
        - Host is a string
        - Port is an integer
        - Catalog is a string
        - User is a string
        - Pass is a string
    - comment(Body) where Body is a string
    - empty_line(S) where S is a string
*/
file([record(Host,Port,Catalog,User,Pass)|Things]) --> record(Host,Port,Catalog,User,Pass), file(Things).
file([comment(Body)|Things]) --> comment(CBody), {string_codes(Body,CBody)}, file(Things).
file([empty_line(S)|Things]) --> empty_line(Cs), {string_codes(S,Cs)}, file(Things).
file([]) --> [].

comment(Body) --> "#", comment_body(Body).

comment_body(A) --> rsep(A).
comment_body([H|T]) --> [H], comment_body(T).

empty_line(A) --> rsep(A).
empty_line([H|T]) --> [H], {code_type(H,white)}, empty_line(T).

/** record(?Record)//

A line in a PostgreSQL pgpass file.
*/
record(Host,Port,Catalog,User,Pass) -->
    record_begin,
    field(Host), fsep,
    field(SPort), {number_string(Port,SPort)}, fsep,
    field(Catalog), fsep,
    field(User), fsep,
    field(Pass), rsep.

record_begin, [C] --> {clp_neq(C, 0'#)}, [C].

field(S) --> fcodes(C), {string_codes(S,C)}.

fcodes([C|Cs]) --> fcode(C), fcodes(Cs).
fcodes([]) --> [].

fcode(C) --> {clp_nmember(C, [0':, 0'\n, 0'\r])}, [C].
fcode(0':) --> "\\:".

clp_neq(A,B) :- when((nonvar(A),nonvar(B)), A \= B).
clp_nmember(A,B) :- when((nonvar(A),nonvar(B)), \+member(A,B)).

fsep --> ":".

rsep(C, A, B) :- rsep(A, B), append(C, B, A).

rsep --> "\n".
rsep --> "\r\n".
