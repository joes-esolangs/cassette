:- module(run, [run/1, run_bare/1, debugc/1]).
:- use_module(eval).
:- use_module('../frontend/parser').
:- use_module(tape).

% run predicates
run(Code) :-
    parse(Code, AST),
    run_bare(AST).

run_bare(AST) :-
    Tape @- !, CTX = ctx{},
    eval_list(AST, CTX, Tape, ECTX, ETape),
    print_term(ETape, []), nl, print_term(ECTX, []).

debugc(Code) :-
    parse(Code, AST),
    gtrace,
    run_bare(AST).

