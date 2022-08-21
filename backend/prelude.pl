:- module(prelude, [prelude/5]).
:- use_module(tape).
:- use_module(library(yall)).
% the overidable builtins

op("+", [X, Y, R] >> (R is X + Y)).

prelude(Op, CTX, Tape, CTX, NTape) :-
    op(Op, F),
    bin_op(F, Tape, NTape).

% utils
bin_pop(Tape, X, Y, NTape) :-
    Tape0 @- Tape^X, NTape @- Tape0^Y.

bin_op(Op, Tape, NTape) :-
    bin_pop(Tape, X, Y, Tape0),
    call(Op, X, Y, R),
    NTape @- Tape0+R.
