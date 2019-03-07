:- module(java0,[
    recordtype_javaclass/2,
    write_class/1
]).
:- use_module('./type.pro').
:- use_module('./type_java.pro').
:- use_module('./syntax.pro').

% deprecated

recordtype_javaconstructor(T,Ctor) :-
    Ctor = [
        access-public,
        parameters-Parameters,
        body-[] % TODO
    ],
    findall(Param, (recordtype_field(T,F),recordtypefield_jctorparam(F,Param)), Parameters).

    recordtypefield_jctorparam(F,[name-Name,type-JType]) :-
        field_name(F,Name),
        field_type(F,T),
        type_javatype(T,JType).

javaclass_access(C,A) :- dictionary_get(C,access,A).
javaclass_name(C,A) :- dictionary_get(C,name,A).
javaclass_final(C) :- dictionary_get(C,final,F), F = true.
javaclass_field(C,F) :- dictionary_get(C,fields,Fs), member(F,Fs).
javaclass_constructor(C,K) :- dictionary_get(C,constructors,Ks), member(K,Ks).

javaclassmember_final(C) :- dictionary_get(C,final,F), F = true.

    dictionary_get(D,K,V) :- member(K-V,D), !.
    dictionary_get(D,K,_) :- throw(error(missing_key(K,D),_)).

% string generation language
lineexp_value(A,B) :- var(A), !, A = B.
lineexp_value([],Z) :- !, Z = "".
lineexp_value([H|T],Z) :- !, lineexp_value(H,H0), lineexp_value(H0+T,Z).
lineexp_value(A+B,C) :- !, lineexp_value(A,A0), lineexp_value(B,B0), string_concat(A0,B0,C).
lineexp_value(D:K,Z) :- !, dictionary_get(D,K,V), lineexp_value(V,Z).
lineexp_value(s(A),B) :- !, lineexp_value(A,T), term_string(T,B,[quoted(false)]).
lineexp_value(join(_,[]),Z) :- !, Z = "".
lineexp_value(join(_,[H]),Z) :- !, lineexp_value(H,Z).
lineexp_value(join(Sep,[H|T]),Z) :- !, lineexp_value(H,H0), lineexp_value(H0+Sep+join(Sep,T),Z).
lineexp_value(if(Cond,T,F),Z) :- !, (evalcond(Cond) -> lineexp_value(T,Z) ; lineexp_value(F,Z)).
lineexp_value(succeed(G),Z) :- call(G) -> Z = true ; Z = false.
lineexp_value(forall(G,P),Z) :- !, findall(P,G,Bag), lineexp_value(Bag,Z).
lineexp_value(line(A),Z) :- !, lineexp_value(A,B), string_concat(B,"\n",Z).
lineexp_value(#A,Z) :- !, Z = #A.
lineexp_value(A,B) :- atom(A), !, term_string(A,S,[quoted(false)]), B=S.
lineexp_value(A,B) :- string(A), !, A = B.
lineexp_value(A,_) :- domain_error(line_expression,A).

    evalcond(A=B) :- !, lineexp_value(A,A0), lineexp_value(B,B0), A0==B0.
    evalcond(A) :- !, lineexp_value(A,#true).

write_class(Class) :-
    javaclass_access(Class,Access),
    javaclass_name(Class,Name),
    Final = if(Class:final=true, "final", ""),
    %findall(Prog, (javaclass_field(Class,Field), write_java_field(Field,Prog)), WriteFields),
    lineexp_value([
        line(join(" ",[s(Access),Final,"class",s(Name),"{"])),
        forall(
            javaclass_field(Class,Field),
            line(["    ", join(" ",[s(Field:access), if(Field:final=true,"final",""), s(Field:type), s(Field:name)]), ";"])
        ),
        %WriteFields,
        line("}")
    ],Str),
    write(Str).
/*
    format("~w ~w class ~w {~n", [Access,Final,Name]),
    forall(javaclass_field(Class,Field), writejavafield(Field)),
    forall(javaclass_constructor(Class,Ctor), write_java_constructor(Name,Ctor)),
    write("}\n").
*/

write_java_constructor(ClassName,Ctor) :-
    % TODO dictionary_get(Ctor,parameters,Parameters),
    dictionary_get(Ctor,access,Access),
    format("    ~w ~w (", [Access,ClassName]),
    format(") {~n"),
    format("    }~n").

javafield_lineexp(Field, [
    "    ", #join(" ", [Field:access, Field:final, Field:type, Field:name]), ";\n"
]).
