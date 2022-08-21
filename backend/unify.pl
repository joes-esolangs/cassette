:- module(unify, [unify/4, unify_list/4]).
% unify patterns

% FIXME: feels weird and uncomplete
unify(fn([lit(Lit)], _), pat_lit(Lit), CTX, CTX).

unify(Lit, pat_lit(Lit), CTX, CTX).

unify(Expr, pat_var(Sym), CTX, NCTX) :-
    atom_string(N, Sym),
    put_assoc(N, CTX, Expr, NCTX).

% FIXME: make it unify better
unify(tape(([Expr], _)), pat_tape([Pat]), CTX, NCTX) :-
    unify_list([Expr], Pat, CTX, NCTX).
unify(tape(([Expr|ERest], _)), pat_tape([Pat|PRest]), CTX, NCTX) :-
    unify_list([Expr], Pat, CTX, CTX0),
    unify(tape((ERest, _)), pat_tape(PRest), CTX0, NCTX).

unify(_, _, _, _) :- false.

% unify list of patterns

unify_list(_, [], _, _) :- true.
unify_list([Expr], [Pat], CTX, NCTX) :-
    unify(Expr, Pat, CTX, NCTX).
unify_list([Expr|ERest], [Pat|PRest], CTX, NCTX) :-
    unify(Expr, Pat, CTX, CTX0),
    unify_list(ERest, PRest, CTX0, NCTX).
