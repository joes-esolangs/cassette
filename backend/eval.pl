:- module(eval, [eval/3, run/1, run_bare/1]).
:- use_module('../frontend/parser').
:- use_module(tape).

eval([], _CTX, _Tape) :- !.

% literals
eval([lit(Lit)|Rest], CTX, Tape) :- !,
    tape:ins(Lit, Tape, NTape),
    eval(Rest, CTX, NTape).

% basic variables
% TODO: add patterns and multiples variable decls
eval([as([pat_var(Pat)])|Rest], CTX, Tape) :-
    tape:get_v(Tape, Val),
    tape:del(Tape, NTape),
    atom_string(N, Pat),
    eval(Rest, CTX.put([N=Val]), NTape).

% run

run(Code) :-
    parse(Code, AST),
    run_bare(AST).

run_bare(AST) :-
    tape:empty(Tape), CTX = ctx{},
    eval(AST, CTX, Tape),
    print_term(Tape, []), nl, print_term(CTX, []).

