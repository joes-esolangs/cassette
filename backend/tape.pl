:- module(tape, [
    empty/1,
    set/3,
    ins/3,
    del/2,
    get_v/2,
    left/2,
    right/2
]).

empty(T) :- T = ([], []).

set(V, ([_|T1], T2), ([V|T1], T2)).

ins(V, (T1, T2), ([V|T1], T2)).

del(([_|Tl], T2), (Tl, T2)).

get_v(([V|_], _), V).

left(([], X), ([], X)).
left(([X], []), ([X], [])).
left(([X], [H|T]), RT) :- reverse([H|T], L), RT = (L, [X]).
left(([H1, H2|T1], T2), ([H2|T1], [H1|T2])).

right((X, [H1|T2]), ([H1|X], T2)).
right(([], []), ([], [])).
right((X, []), RT) :- reverse(X, [H|T]), RT = ([H], T).


