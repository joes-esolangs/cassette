:- module(eval, [eval/5, eval_list/4, run/1, run_bare/1]).
:- use_module('../frontend/parser').
:- use_module(tape).
:- use_module(constructs).

% literals
eval(lit(Lit), CTX, Tape, CTX, NTape) :-
    NTape @- Tape+Lit.

eval(as(Pats), CTX, Tape, NCTX, NTape) :-
    as_c(Pats, CTX, Tape, NCTX, NTape).

eval(sym(Name), CTX, Tape, CTX, NTape) :-
    atom_string(N, Name),
    NTape @- Tape+CTX.get(N).

% TODO
eval(tape(Exprs), CTX, Tape, CTX, NTape) :- true.

% make if sugar for a case where the condition is true or false

% evaluating a list of instructions
eval_list([], CTX, Tape, (CTX, Tape)).
eval_list([I|Rest], CTX, Tape, Res) :-
    eval(I, CTX, Tape, NCTX, NTape),
    eval_list(Rest, NCTX, NTape, Res).

% run predicates
run(Code) :-
    parse(Code, AST),
    run_bare(AST).

run_bare(AST) :-
    Tape @- !, CTX = ctx{},
    eval_list(AST, CTX, Tape, Res),
    (ECTX, ETape) = Res,
    print_term(ETape, []), nl, print_term(ECTX, []).


