:- module(constructs, [
              as_c/5
          ]).
:- use_module(tape).
:- use_module(unify).

as_c([Pat], CTX, Tape, NCTX, NTape) :-
    Val @- @Tape,
    NTape @- \Tape,
    unify(Val, Pat, CTX, NCTX).
as_c([Pat|Rest], CTX, Tape, NCTX, NTape) :-
    as_c([Pat], CTX, Tape, CTX0, Tape0),
    as_c(Rest, CTX0, Tape0, NCTX, NTape).

