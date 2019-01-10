:- module(dcg_common, [
    spaces1//1,
    space//1,
    reluctant//1
]).

/** <module> Common DCG rules

*/

spaces1([H]) --> space(H).
spaces1([H|T]) --> space(H), spaces1(T).

space(S) --> [S], {code_type(S,space)}.

reluctant([]) --> [].
reluctant([H|T]) --> [H], reluctant(T).
