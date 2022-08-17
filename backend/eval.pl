:- module(eval, [eval/4, run/1, run_bare/1]).
:- use_module('../frontend/parser').
:- use_module(tape).
:- use_module(unify).

eval([], CTX, Tape, (CTX, Tape)).

% literals
eval([lit(Lit, Type)|Rest], CTX, Tape, Res) :-
    NTape @- Tape+lit(Lit, Type),
    eval(Rest, CTX, NTape, Res).

% basic variables
% TODO: add patterns and multiples variable decls
eval([as([Pat])|Rest], CTX, Tape, Res) :-
    Val @- @Tape,
    NTape @- \Tape,
    % FIXME: bug unifying two lits of the same type. maybe type literals in the lexer: int_lit, string_lit
    unify(Val, Pat, CTX, NCTX),
    eval(Rest, NCTX, NTape, Res).

eval([sym(Name)|Rest], CTX, Tape, Res) :-
    atom_string(N, Name),
    NTape @- Tape+CTX.get(N),
    eval(Rest, CTX, NTape, Res).

% make if sugar for a case where the condition is true or false

% run

run(Code) :-
    parse(Code, AST),
    run_bare(AST).

run_bare(AST) :-
    Tape @- !, CTX = ctx{},
    eval(AST, CTX, Tape, Res),
    (ECTX, ETape) = Res,
    print_term(ETape, []), nl, print_term(ECTX, []).


