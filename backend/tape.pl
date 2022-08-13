:- module(tape, [
    % fix operators
    op(700, xfx, @-),
    op(500, xfy, +),
    op(500, xfy, =),
    op(500, xf, \),
    op(500, fx, @),
    op(500, fx, ->),
    op(500, fx, <-),
    (@-)/2,
    empty/1,
    set/3,
    ins/3,
    del/2,
    get_v/2,
    left/2,
    right/2
]).

@-(Res, !) :-
    empty(T), Res = T.
@-(Res, T+V) :-
    ins(V, T, NT), Res = NT.
@-(Res, T=V) :-
    set(V, T, NT), Res = NT.
@-(Res, \T) :-
    del(T, NT), Res = NT.
@-(Res, @T) :-
    get_v(T, V), Res = V.
@-(Res, ->T) :-
    left(T, NT), Res = NT.
@-(Res, <-T) :-
    right(T, NT), Res = NT.

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


