:- module(eval, [eval/5, eval_list/5]).
:- use_module('../frontend/parser').
:- use_module(tape).
:- use_module(unify).

% constructs
as_c([Pat], CTX, Tape, NCTX, NTape) :-
    (   Val @- @Tape, NTape @- \Tape
    ;   throw("error evaluating 'as'")),
    unify(Val, Pat, CTX, NCTX).
as_c([Pat|Rest], CTX, Tape, NCTX, NTape) :-
    as_c([Pat], CTX, Tape, CTX0, Tape0),
    as_c(Rest, CTX0, Tape0, NCTX, NTape).

% TODO: should i not reverse the left stack?
tape_c([], CTX, (L, R), CTX, MTape) :- reverse(L, NL), MTape = (NL, R).
% eacg element is a cassette function on the stack
tape_c([Exprs|Rest], CTX, Tape, NCTX, NTape) :-
    Tape0 @- Tape+fn(Exprs, CTX),
    tape_c(Rest, CTX, Tape0, NCTX, NTape).

quote_c(AST, (AST, [])).

% make if sugar for a case where the condition is true or false
%make cond sugar for a case with a bunch of when clauses

% evalutation
eval(sym("~>"), CTX, Tape, NCTX, NTape) :-
    (   quote(Quote) @- @Tape, Tape0 @- \Tape
    ;   throw("value is not a quote or there isn't enough values")),
    to_list(Quote, Ins),
    eval_list(Ins, CTX, Tape0, NCTX, NTape).

eval(sym(Name), CTX, Tape, NCTX, NTape) :-
    atom_string(N, Name),
    get_assoc(N, CTX, fn(AST, FCTX)),
    eval_list(AST, FCTX, Tape, CTX0, NTape),
    merge_ctx(CTX, CTX0, NCTX).
eval(sym(Name), CTX, Tape, CTX, NTape) :-
    atom_string(N, Name),
    get_assoc(N, CTX, V),
    NTape @- Tape+V.

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

% FIXME
% evaluating a list of instructions
eval_list([], CTX, Tape, CTX, Tape).
eval_list([I|Rest], CTX, Tape, NCTX, NTape) :-
    eval(I, CTX, Tape, CTX0, Tape0),
    eval_list(Rest, CTX0, Tape0, NCTX, NTape).

% utils
merge_ctx(A1, A2, R) :-
    assoc_to_list(A1, L1),
    assoc_to_list(A2, L2),
    append(L1, L2, L3),
    list_to_assoc(L3, R).

