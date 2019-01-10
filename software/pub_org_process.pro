:- module(pub_org_process, [
    process_footnotes/4
]).

/** process_footnotes(+Footnotes0, +Ast0, -Footnotes1, -Ast1)

Collect and number footnotes.
*/
process_footnotes(F0,A0,F1,A1) :-
    A0=[] ->  F0=F1, A0=A1
;   A0=[H|T] -> A1=[A1H|A1T]
        , process_footnotes(F0,H,F,A1H)
        , process_footnotes(F,T,F1,A1T)
;   A0=fi(Name,Text) -> A1=fnref(N1), fn_upsert(Name,Text,F0,1,F1,N1)
;   A0=paragraph(Inlines0) -> A1=paragraph(Inlines1), process_footnotes(F0,Inlines0,F1,Inlines1)
;   A0=s(_) -> F1=F0, A1=A0.

fn_upsert(Name,Text,F0,N0,F1,N1) :-
    F0=[] -> F1=[f(N0,Name,Text)], N1=N0
;   Name\='', F0=[f(N1,Name,nil)|F0T] -> F1=[f(N1,Name,Text)|F0T]
;   Name\='', F0=[f(N1,Name,_)|_] -> F1=F0
;   F0=[F0H|F0T] -> N01 is N0+1, F1=[F0H|F1T], fn_upsert(Name,Text,F0T,N01,F1T,N1).
