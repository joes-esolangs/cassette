:- module(unify, [unify/4]).

% unify patterns

unify(lit(Lit, Type), pat_lit(lit(Lit, Type)), CTX, CTX).

unify(Expr, pat_var(Sym), CTX, NCTX) :-
    atom_string(N, Sym),
    NCTX = CTX.put(N, Expr).
