:- module(unify, [unify/4, unify_list/4]).
% unify patterns
unify(Lit, pat_lit(Lit), CTX, CTX).

unify(lit(Expr), pat_var(Sym), CTX, NCTX) :-
    atom_string(N, Sym),
    NCTX = CTX.put(N, Expr).

% FIXME: this unification exceeds stack limit
unify(tape(Exprs), pat_tape(Pats), CTX, NCTX) :-
    length(Exprs, L), length(Pats, L),
    unify_list(Exprs, Pats, CTX, NCTX).

unify(_, _, _, _) :- false.

% unify list of patterns

unify_list(_, [], _, _) :- true.
unify_list([Expr], [Pat], CTX, NCTX) :-
    unify(Expr, Pat, CTX, NCTX).
unify_list([Expr|ERest], [Pat|PRest], CTX, NCTX) :-
    unify(Expr, Pat, CTX, CTX0),
    unify_list(ERest, PRest, CTX0, NCTX).
