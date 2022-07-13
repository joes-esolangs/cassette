:- module(parser, [parse/2]).
:- use_module(lexer).
% add pattern matching for arguments

parse(Code, AST) :-
    tokenize(Code, Out),
    % group functions here
    instructions(AST, Out, []).

instruction(N) --> fn_def(N); lam_def(N); lit(N); sym(N); seq(N); pass(N); tracer.

instructions([N]) --> instruction(N).
instructions([N|R]) --> instruction(N), instructions(R).

tracer --> [sym_t("$trace",_)], {trace}.

fn_def(fn(Name, Args, Instructions, [LL, NL, AL, EL])) --> ['fn'(LL), sym_t(Name, NL)], arg_list(Args), ['::'(AL)], instructions(Instructions), ['end'(EL)].
fn_def(fn(Name, [], Instructions, [LL, NL, AL, EL])) --> ['fn'(LL), sym_t(Name, NL), '::'(AL)], instructions(Instructions), ['end'(EL)].

lam_def(lam(Args, Instructions, [LL, AL, EL])) --> ['lam'(LL)], arg_list(Args), ['::'(AL)], instructions(Instructions), ['end'(EL)].
lam_def(lam([], Instructions, [LL, AL, EL])) --> ['lam'(LL), '::'(AL)], instructions(Instructions), ['end'(EL)].
lam_def(lam([], Instructions, [L1, L2])) --> ['('(L1)], instructions(Instructions), [')'(L2)].

pass(pass(L)) --> ['pass'(L)].

lit(lit(V, L)) --> [lit_t(V, L)].
sym(sym(N, L)) --> [sym_t(N, L)].

elem(N)               --> lit(N); sym(N); seq(N); lam_def(N).
elem_group(N)         --> elem(N).
elem_group([N|R])     --> elem(N), elem_group(R).
elems(N, _)           --> elem_group(N).
elems([N|R], list)    --> elem_group(N), [','(_)], elems(R, list).
elems([N|R], tape)    --> elem_group(N), ['~'(_)], elems(R, tape).
seq(list([], L1, L2)) --> ['['(L1), ']'(L2)].
seq(tape([], L1, L2)) --> ['['(L1), '~'(_), ']'(L2)].
seq(list(N, L1, L2))  --> ['['(L1)], elems(N, list), [']'(L2)]. % linked list
seq(tape(N, L1, L2))  --> ['['(L1)], elems(N, tape), [']'(L2)]. % circular doubly linked list

arg_list(P)   --> pattern(P).
arg_list([P|R]) --> pattern(P), arg_list(R).

pat_list(P)         --> pattern(P).
pat_list([P|R])     --> pattern(P), pat_list(R).
pattern(pat_lit(V)) --> [lit_t(V, _)].
pattern(pat_var(N)) --> [sym_t(N, _)].
pattern(pat_cons(H, T)) --> ['['(_)], pat_list(H), [sym_t("<:",_)], pattern(T), [']'(_)].
pattern(pat_snoc(H, T)) --> ['['(_)], pat_list(H), [sym_t("<:",_)], pattern(T), [']'(_)]. % only works for tapes
pattern(pat_list(L)) --> ['['(_)], pat_list(L), [']'(_)].

