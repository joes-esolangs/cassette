:- module(unify, [unify/3, unify/4]).

% unify patterns

unify(lit(Lit), pat_lit(Lit), _CTX).

unify(Expr, pat_var(Sym), CTX, NCTX) :-
    atom_string(N, Sym),
    NCTX = CTX.put(N, Expr).
