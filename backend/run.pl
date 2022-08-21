:- module(run, [run/1, run_bare/1, debugc/1]).
:- use_module(eval).
:- use_module('../frontend/parser').
:- use_module(tape).

% run predicates
run(Code) :-
    parse(Code, AST),
    run_bare(AST).

run_bare(AST) :-
    Tape @- !, empty_assoc(CTX),
    eval_list(AST, CTX, Tape, NCTX, ETape),
    assoc_to_list(NCTX, ECTX),
    print_term(ETape, []), nl, print_term(ECTX, []).

debugc(Code) :-
    parse(Code, AST),
    gtrace,
    run_bare(AST).

