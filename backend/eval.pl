:- module(eval, [eval/5, eval_list/5]).
:- use_module(tape).
:- use_module(unify).
:- use_module(prelude).

% constructs
as_c([], CTX, Tape, CTX, Tape).
as_c([Pat], CTX, Tape, NCTX, NTape) :-
    NTape @- Tape^Val,
    unify(Val, Pat, CTX, NCTX).
as_c([Pat|Rest], CTX, Tape, NCTX, NTape) :-
    as_c([Pat], CTX, Tape, CTX0, Tape0),
    as_c(Rest, CTX0, Tape0, NCTX, NTape).

tape_c([], CTX, (L, R), CTX, MTape) :- reverse(L, NL), MTape = (NL, R).
tape_c([Exprs|Rest], CTX, Tape, NCTX, NTape) :-
    Tape0 @- Tape+fn(Exprs, CTX),
    tape_c(Rest, CTX, Tape0, NCTX, NTape).

quote_c(AST, (AST, [])).

% make if sugar for a case where the condition is true or false
%make cond sugar for a case with a bunch of when clauses

% evalutation
eval(sym("~>"), CTX, Tape, NCTX, NTape) :-
    (   Tape0 @- Tape^quote(Quote)
    ;   throw("value is not a quote or there isn't enough values")), % TODO: maybe remove throw and give false
    to_list(Quote, Ins),
    eval_list(Ins, CTX, Tape0, NCTX, NTape).

eval(sym("pass"), CTX, Tape, CTX, Tape).

eval(sym(Name), CTX, Tape, CTX, NTape) :-
    atom_string(N, Name),
    fn(AST, FCTX) = CTX.get(N),
    eval_list(AST, FCTX, Tape, _CTX, NTape).
eval(sym(Name), CTX, Tape, CTX, NTape) :-
    atom_string(N, Name),
    (   NTape @- Tape+CTX.get(N)
    ; throw(format('unknown symbol ~a', [N]))). % TODO: maybe remove throw and give false

eval(sym(Name), CTX, Tape, NCTX, NTape) :-
    prelude(Name, CTX, Tape, NCTX, NTape).

eval(lit(Lit), CTX, Tape, CTX, NTape) :-
    NTape @- Tape+Lit.

eval(as(Pats), CTX, Tape, NCTX, NTape) :-
    as_c(Pats, CTX, Tape, NCTX, NTape).

eval(tape(Exprs), CTX, Tape, NCTX, NTape) :-
    tape_c(Exprs, CTX, Tape, NCTX, MTape),
    NTape @- Tape+tape(MTape).

eval(quote(AST), CTX, Tape, CTX, NTape) :-
    quote_c(AST, Quote),
    NTape @- Tape+quote(Quote).

eval(fn(Name, Args, _When, Body), CTX, Tape, NCTX, Tape) :-
    AST = [as(Args)|Body],
    atom_string(N, Name),
    NCTX = CTX.put(N, fn(AST, CTX)).

% evaluating a list of instructions
eval_list([], CTX, Tape, CTX, Tape).
eval_list([I|Rest], CTX, Tape, NCTX, NTape) :-
    eval(I, CTX, Tape, CTX0, Tape0),
    eval_list(Rest, CTX0, Tape0, NCTX, NTape).

% utils

% used to be something here
