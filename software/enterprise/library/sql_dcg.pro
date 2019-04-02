select(Schema,Table,Cols) -->
    "SELECT ", cols(Cols), " FROM ", table(Schema,Table).

cols([]) --> "".
cols([A]) --> !, col(A).
cols([A|B]) --> col(A), ", ", cols(B).

col(A) --> s(A).

table(S, T) --> s(S), ".", s(T).

s(A) --> {atom_string(A,S)}, S.
