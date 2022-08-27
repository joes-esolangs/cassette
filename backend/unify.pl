:- module(unify, [unify/4, unify_list/4]).
:- use_module(tape).

% unify patterns

unify(Lit, pat_lit(Lit), CTX, CTX).

% TODO: make non linear. if sym is already in ctx, use its value
unify(Expr, pat_var(Sym), CTX, NCTX) :-
    atom_string(N, Sym),
    NCTX = CTX.put(N, Expr).

unify(tape(([fn(Expr, FCTX)], _)), pat_tape([Pat]), CTX, NCTX) :-
    fn_list(Expr, Pat, FCTX, CTX, NCTX).
unify(tape(([fn(Expr, FCTX)|ERest], _)), pat_tape([Pat|PRest]), CTX, NCTX) :-
    fn_list(Expr, Pat, FCTX, CTX, CTX0),
    unify(tape((ERest, _)), pat_tape(PRest), CTX0, NCTX).

unify(quote(Tape), pat_quote(Sym), CTX, NCTX) :-
    to_list(Tape, AST),
    unify(fn(AST, CTX), pat_var(Sym), CTX, NCTX).

unify(_, _, _, _) :- !, fail.

% unify list of patterns

unify_list([Expr], [Pat], CTX, NCTX) :-
    unify(Expr, Pat, CTX, NCTX).
unify_list([Expr|ERest], [Pat|PRest], CTX, NCTX) :-
    unify(Expr, Pat, CTX, CTX0),
    unify_list(ERest, PRest, CTX0, NCTX).

fn_list([], [_|_], _, _, _) :- fail.
fn_list([E|R], [pat_var(Sym)], FCTX, CTX, NCTX) :-
    unify_fn([E|R], pat_var(Sym), FCTX, CTX, NCTX).
fn_list([Expr], [Pat], FCTX, CTX, NCTX) :-
    unify_fn([Expr], Pat, FCTX, CTX, NCTX).
fn_list([Expr|ERest], [Pat|PRest], FCTX, CTX, NCTX) :-
    unify_fn([Expr], Pat, FCTX, CTX, CTX0),
    fn_list(ERest, PRest, FCTX, CTX0, NCTX).

unify_fn([lit(Lit)], pat_lit(Lit), _, CTX, CTX).
unify_fn(Expr, pat_var(Sym), FCTX, CTX, NCTX) :-
    unify(fn(Expr, FCTX), pat_var(Sym), CTX, NCTX).

% TODO: make unify_stack predicate
