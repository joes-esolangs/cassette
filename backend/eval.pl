:- module(eval, [eval/4, run/1, run_bare/1]).
:- use_module('../frontend/parser').
:- use_module(tape).

% eval(AST, CTX, Tape, Res) :- eval(AST, CTX, Tape), Res = (CTX, Tape).

eval([], CTX, Tape, (CTX, Tape)).

% literals
eval([lit(Lit)|Rest], CTX, Tape, Res) :-
    tape:ins(Lit, Tape, NTape),
    eval(Rest, CTX, NTape, Res).

% basic variables
% TODO: add patterns and multiples variable decls
eval([as([pat_var(Pat)])|Rest], CTX, Tape, Res) :-
    tape:get_v(Tape, Val),
    tape:del(Tape, NTape),
    atom_string(N, Pat),
    eval(Rest, CTX.put([N=Val]), NTape, Res).

% run

run(Code) :-
    parse(Code, AST),
    run_bare(AST).

run_bare(AST) :-
    tape:empty(Tape), CTX = ctx{},
    eval(AST, CTX, Tape, Res),
%    (CTX, Tape) = Res,
%    print_term(Tape, []), nl, print_term(CTX, []).
     print_term(Res, []).

