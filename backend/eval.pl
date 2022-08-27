:- module(eval, [eval/5, eval_list/5]).
:- use_module(tape).
:- use_module(unify).
:- use_module(builtins).

% constructs
as_c([], CTX, Tape, CTX, Tape).
as_c([Pat], CTX, Tape, NCTX, NTape) :-
    NTape @- Tape^Val,
    !, unify(Val, Pat, CTX, NCTX).
as_c([Pat|Rest], CTX, Tape, NCTX, NTape) :-
    as_c([Pat], CTX, Tape, CTX0, Tape0),
    as_c(Rest, CTX0, Tape0, NCTX, NTape).

tape_group([], fn([], CTX), CTX).
tape_group([tape(Exprs)|ERest], fn([tape(Tape)|VRest], CTX), CTX) :-
    Empty @- !, tape_c(Exprs, CTX, Empty, Tape),
    tape_group(ERest, fn(VRest, CTX), CTX).
tape_group([Expr|ERest], fn([Expr|VRest], CTX), CTX) :- tape_group(ERest, fn(VRest, CTX), CTX).

tape_c([], _CTX, (L, R), MTape) :- reverse(L, NL), MTape = (NL, R).
tape_c([Exprs|Rest], CTX, Tape, NTape) :-
    tape_group(Exprs, Fn, CTX),
    Tape0 @- Tape+Fn,
    tape_c(Rest, CTX, Tape0, NTape).

case_eval_c([], _CTX1, CTX2, Tape, CTX2, Tape).
case_eval_c([Expr|ERest], CTX1, CTX2, Tape, NCTX, NTape) :-
    (   eval(Expr, CTX1, Tape, NCTX1, NTape)
    ;   eval(Expr, CTX2, Tape, NCTX2, NTape)),
    case_eval_c(ERest, NCTX1, NCTX2, Tape, NCTX, NTape).

% maybe use it later? need to support multiple expressions and pattern
% TODO: fix
case_c(Expr, branch(Pat, _When, Ins), CTX, Tape, NCTX, NTape) :-
    eval(Expr, CTX, Tape, CTX0, Tape0),
    as_c(Pat, CTX0, Tape0, CTX1, NTape),
    case_eval_c(Ins, CTX1, CTX0, Tape0, NCTX, NTape).

quote_c(AST, (AST, [])).

% make if sugar for a case where the condition is true or false
% make cond sugar for a case with a bunch of when clauses

% evalutation
eval(case(_, [], []), CTX, Tape, CTX, Tape).
eval(case(_, [], Else), CTX, Tape, NCTX, NTape) :-
    eval_list(Else, CTX, Tape, NCTX, NTape).
eval(case(Expr, [Branch|BRest], Else), CTX, Tape, NCTX, NTape) :-
    (   Tape0 @- \Tape, case_c(Expr, Branch, CTX, Tape0, NCTX, NTape)
    ;   eval(case(Expr, BRest, Else), CTX, Tape, NCTX, NTape)).

eval(sym("pass"), CTX, Tape, CTX, Tape).
eval(sym("trace!"), CTX, Tape, CTX, Tape) :- trace.
eval(sym("gtrace!"), CTX, Tape, CTX, Tape) :- gtrace.

eval(sym(Name), CTX, Tape, CTX, NTape) :-
    atom_string(N, Name),
    fn(AST, FCTX) = CTX.get(N),
    eval_list(AST, FCTX, Tape, _CTX, NTape).
eval(sym(Name), CTX, Tape, CTX, NTape) :-
    atom_string(N, Name),
    NTape @- Tape+CTX.get(N).

eval(sym(Name), CTX, Tape, NCTX, NTape) :-
    builtin(Name, CTX, Tape, NCTX, NTape); fail.

eval(lit(Lit), CTX, Tape, CTX, NTape) :-
    NTape @- Tape+Lit.

eval(as(Pats), CTX, Tape, NCTX, NTape) :-
    as_c(Pats, CTX, Tape, NCTX, NTape).

eval(tape(Exprs), CTX, Tape, CTX, NTape) :-
    tape_c(Exprs, CTX, Tape, MTape),
    NTape @- Tape+tape(MTape).

eval(quote(AST), CTX, Tape, CTX, NTape) :-
    quote_c(AST, Quote),
    NTape @- Tape+quote(Quote).

eval(fn(Name, Args, _When, Body), CTX, Tape, NCTX, Tape) :-
    AST = [as(Args)|Body],
    atom_string(N, Name),
    FCTX = CTX.put(N, fn(AST, FCTX)),
    NCTX = CTX.put(N, fn(AST, FCTX)).

% evaluating a list of instructions
eval_list([], CTX, Tape, CTX, Tape).
eval_list([I|Rest], CTX, Tape, NCTX, NTape) :-
    eval(I, CTX, Tape, CTX0, Tape0),
    eval_list(Rest, CTX0, Tape0, NCTX, NTape).

% utils

% used to be something here
