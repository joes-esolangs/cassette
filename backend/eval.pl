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

% maybe use it later? need to support multiple expressions and patterns
case_c(Expr, branch(Pat, _When, Ins), CTX, Tape, NCTX, NTape) :-
    eval(Expr, CTX, Tape, CTX0, Tape0),
    as_c([Pat], CTX0, Tape0, CTX1, _Tape),
    Empty @- !,
    eval2_list(Ins, CTX1, CTX0, Empty, NCTX, NTape).

%unquote_c(unquote(Expr), Out, CTX) :- unquote_c(Expr, Out, CTX).
%unquote_c(splice(Expr), Out, CTX) :- splice_c(Expr, [], CTX, Out).
unquote_c(Expr, Out, CTX) :-
    Empty @- !,
    eval(Expr, CTX, Empty, _, NTape), to_list(NTape, [Out]).

splice_c([], Out, _, Out).
splice_c([Expr|ERest], Out, CTX, Res) :-
    Empty @- !,
    eval_list(Expr, CTX, Empty, _, NTape),
    to_list(NTape, A), append(Out, A, NOut),
    splice_c(ERest, NOut, CTX, Res).

% TODO: nested quasiquoting https://docs.racket-lang.org/guide/qq.html.
% evaluate them
quasiquote_c([], Out, _, Out).
quasiquote_c([unquote(E)|In], Out, CTX, Res) :-
    unquote_c(E, O, CTX),
    (   is_list(O), append(Out, O, Out1)
    ;   append(Out, [O], Out1)),
    quasiquote_c(In, Out1, CTX, Res).
quasiquote_c([splice(Exprs)|In], Out, CTX, Res) :-
    splice_c(Exprs, [], CTX, O),
    append(Out, O, NOut),
    quasiquote_c(In, NOut, CTX, Res).
quasiquote_c([E|In], Out, CTX, Res) :-
    append(Out, [E], Out1),
    quasiquote_c(In, Out1, CTX, Res).

quote_c(AST, (AST, [])).

% make if sugar for a case where the condition is true or false
% make cond sugar for a case with a bunch of when clauses

% evalutation
eval(case(_, [], []), CTX, Tape, CTX, Tape).
eval(case(_, [], Else), CTX, Tape, NCTX, NTape) :-
    eval_list(Else, CTX, Tape, NCTX, NTape).
eval(case(Expr, [Branch|BRest], Else), CTX, Tape, NCTX, NTape) :-
    (   case_c(Expr, Branch, CTX, Tape, NCTX, NTape)
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
    NTape @- Tape+lit(Lit).

eval(as(Pats), CTX, Tape, NCTX, NTape) :-
    as_c(Pats, CTX, Tape, NCTX, NTape).

eval(tape(Exprs), CTX, Tape, CTX, NTape) :-
    tape_c(Exprs, CTX, Tape, MTape),
    NTape @- Tape+tape(MTape).

eval(quasiquote(AST), CTX, Tape, CTX, NTape) :-
    quasiquote_c(AST, [], CTX, Q),
    to_tape(Q, Quote),
    NTape @- Tape+quote(Quote).

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

eval2_list([], _CTX1, CTX2, Tape, CTX2, Tape).
eval2_list([I|Rest], CTX1, CTX2, Tape, NCTX, NTape) :-
    eval2(I, CTX1, CTX2, Tape, CTX3, Tape0),
    eval2_list(Rest, CTX1, CTX3, Tape0, NCTX, NTape).

eval2(as(Pats), _CTX1, CTX2, Tape, NCTX, NTape) :-
    eval(as(Pats), CTX2, Tape, NCTX, NTape).
eval2(sym(Name), CTX1, CTX2, Tape, CTX2, NTape) :-
    (   eval(sym(Name), CTX1, Tape, _CTX, NTape)
    ;   eval(sym(Name), CTX2, Tape, _CTX, NTape)).
eval2(Expr, _CTX1, CTX2, Tape, NCTX, NTape) :-
    eval(Expr, CTX2, Tape, NCTX, NTape).

% utils

% used to be something here
