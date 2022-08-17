:- module(unify, [unify/4]).

% unify patterns

unify(Lit, pat_lit(Lit), CTX, CTX).

unify(Expr, pat_var(Sym), CTX, NCTX) :-
    atom_string(N, Sym),
    NCTX = CTX.put(N, Expr).
