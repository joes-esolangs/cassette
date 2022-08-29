:- module(builtins, [builtin/5]).
:- use_module(tape).
:- use_module(library(yall)).
:- use_module(pretty_print).

:- discontiguous builtins:def_op/3.

% the overidable builtins

%!  math
def_op(bin, "+", [X, Y, R] >> (R is X + Y)).
def_op(bin, "-", [X, Y, R] >> (R is X - Y)).
def_op(bin, "*", [X, Y, R] >> (R is X * Y)).
def_op(bin, "/", [X, Y, R] >> (R is X / Y)).
def_op(bin, "mod", [X, Y, R] >> (R is X mod Y)).
def_op(bin, "^", [X, Y, R] >> (R is X ^ Y)).

%!  comparison
def_op(bin, ">", [X, Y, R] >> (X > Y, R = yes; R = no)).
def_op(bin, "<", [X, Y, R] >> (X < Y, R = yes; R = no)).
def_op(bin, ">=", [X, Y, R] >> (X >= Y, R = yes; R = no)).
def_op(bin, "=<", [X, Y, R] >> (X =< Y, R = yes; R = no)).
def_op(bin, "/=", [X, Y, R] >> (X \= Y, R = yes; R = no)).
def_op(bin, "=", [X, Y, R] >> (X = Y, R = yes; R = no)).

%!  io
def_op(unit, "out", [X] >> pretty_print(X)).
def_op(unit, "outn", [X] >> (pretty_print(X), nl)).

builtin(Op, CTX, Tape, CTX, NTape) :-
    def_op(Ty, Op, F),
    (   Ty = bin, bin_op(F, Tape, NTape)
    ;   Ty = unit, unit_op(F, Tape, NTape)).

% utils
bin_pop(Tape, X, Y, NTape) :-
    Tape0 @- Tape^lit(X), NTape @- Tape0^lit(Y).

bin_op(Op, Tape, NTape) :-
    bin_pop(Tape, X, Y, Tape0),
    call(Op, Y, X, R),
    NTape @- Tape0+lit(R).

un_op(Op, Tape, NTape) :-
    Tape0 @- Tape^X,
    call(Op, X, R),
    NTape @- Tape0+lit(R).

unit_op(Op, Tape, NTape) :-
    NTape @- Tape^X,
    call(Op, X).

