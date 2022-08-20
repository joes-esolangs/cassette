:- module(eval, [eval/5, eval_list/5]).
:- use_module('../frontend/parser').
:- use_module(tape).
:- use_module(unify).

% constructs
as_c([Pat], CTX, Tape, NCTX, NTape) :-
    Val @- @Tape,
    NTape @- \Tape,
    unify(Val, Pat, CTX, NCTX).
as_c([Pat|Rest], CTX, Tape, NCTX, NTape) :-
    as_c([Pat], CTX, Tape, CTX0, Tape0),
    as_c(Rest, CTX0, Tape0, NCTX, NTape).

% FIXME
tape_c([], CTX, Tape, CTX, Tape).
tape_c([Expr|Rest], CTX, Tape, NCTX, NTape) :-
    Empty @- !,
    eval_list(Expr, CTX, Empty, CTX0, Tape0),
    Tape1 @- Tape+Tape0,
    tape_c(Rest, CTX0, Tape1, NCTX, NTape).
tape_c([Expr|Rest], CTX, Tape, NCTX, NTape) :-
    eval(Expr, CTX, Tape, CTX0, Tape0),
    tape_c(Rest, CTX0, Tape0, NCTX, NTape).

% make if sugar for a case where the condition is true or false
%make cond sugar for a case with a bunch of when clauses

% evalutation
eval(lit(Lit), CTX, Tape, CTX, NTape) :-
    NTape @- Tape+Lit.

eval(as(Pats), CTX, Tape, NCTX, NTape) :-
    as_c(Pats, CTX, Tape, NCTX, NTape).

eval(sym(Name), CTX, Tape, CTX, NTape) :-
    atom_string(N, Name),
    NTape @- Tape+CTX.get(N).

eval(tape(Exprs), CTX, Tape, NCTX, NTape) :-
    tape_c(Exprs, CTX, Tape, NCTX, MTape),
    NTape @- Tape+MTape.

% FIXME
% evaluating a list of instructions
eval_list([], CTX, Tape, CTX, Tape).
eval_list([I|Rest], CTX, Tape, NCTX, NTape) :-
    eval(I, CTX, Tape, CTX0, Tape0),
    eval_list(Rest, CTX0, Tape0, NCTX, NTape).



