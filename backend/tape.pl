:- module(tape, [
    % fix operators
    op(700, xfx, @-),
    op(500, xfy, +),
    op(500, xfy, =),
    op(500, xf, \),
    op(500, fx, @),
    op(500, fx, ->),
    op(500, fx, <-),
    op(500, xfy, ++),
    op(500, xfy, ^),
    is_tape/1,
    to_tape/2,
    to_list/2,
    (@-)/2,
    empty/1,
    set/3,
    ins/3,
    del/2,
    get_v/2,
    left/2,
    right/2,
    cat/3,
    pop/3
]).

is_tape((Left, Right)) :- is_list(Left), is_list(Right).

to_tape(List, (List, [])) :- is_list(List).

to_list((L, R), List) :-
    reverse(R, R_rev),
    append(L, R_rev, List).

@-(Res, !) :-
    empty(Res).
@-(Res, T+V) :-
    ins(V, T, Res).
@-(Res, T=V) :-
    set(V, T, Res).
@-(Res, \T) :-
    del(T, Res).
@-(Res, @T) :-
    get_v(T, Res).
@-(Res, ->T) :-
    left(T, Res).
@-(Res, <-T) :-
    right(T, Res).
@-(Res, T1++T2) :-
    cat(T1, T2, Res).
@-(Res, T^V) :-
    pop(T, V, Res).

empty(T) :- T = ([], []).

set(V, ([_|T1], T2), ([V|T1], T2)).

ins(V, (T1, T2), ([V|T1], T2)).

del(([], _), _) :- false.
del(([_], T2), (L, [])) :- reverse(T2, L).
del(([_|T1], T2), (T1, T2)).

get_v(([], _), _) :- false.
get_v(([V|_], _), V).

left(([], X), ([], X)).
left(([X], []), ([X], [])).
left(([X], [H|T]), (L, [X])) :- reverse([H|T], L).
left(([H1, H2|T1], T2), ([H2|T1], [H1|T2])).

right((X, [H1|T2]), ([H1|X], T2)).
right(([], []), ([], [])).
right((X, []), ([H], T)) :- reverse(X, [H|T]).

cat((XL, XR), (YL, YR), (XY, YR)) :-
    reverse(XR, XR_rev),
    append(XL, XR_rev, X),
    append(X, YL, XY).

pop(T, V, RT) :-
    V @- @T,
    RT @- \T.
