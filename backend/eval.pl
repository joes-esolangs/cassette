:- module(eval, [eval/4, run/1, run_bare/1]).
:- use_module('../frontend/parser').
:- use_module(tape).
:- use_module(constructs).

eval([], CTX, Tape, (CTX, Tape)).

% literals
eval([lit(Lit)|Rest], CTX, Tape, Res) :-
    NTape @- Tape+Lit,
    eval(Rest, CTX, NTape, Res).

eval([as(Pats)|Rest], CTX, Tape, Res) :-
    as_c(Pats, CTX, Tape, NCTX, NTape),
    eval(Rest, NCTX, NTape, Res).

eval([sym(Name)|Rest], CTX, Tape, Res) :-
    atom_string(N, Name),
    NTape @- Tape+CTX.get(N),
    eval(Rest, CTX, NTape, Res).

% make if sugar for a case where the condition is true or false

% run predicates
run(Code) :-
    parse(Code, AST),
    run_bare(AST).

run_bare(AST) :-
    Tape @- !, CTX = ctx{},
    eval(AST, CTX, Tape, Res),
    (ECTX, ETape) = Res,
    print_term(ETape, []), nl, print_term(ECTX, []).


