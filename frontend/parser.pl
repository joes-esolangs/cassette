:- module(parser, [parse/2, pparse/1]).
:- use_module(lexer).
% TODO: for errors pass an error list through every clause.
% IDEA: maybe infix/unary for the builtins

parse(Code, AST) :-
    tokenize(Code, Out),
    instructions(AST, Out, []).

pparse(Code) :-
    parse(Code, AST),
    print_term(AST, []).

instructions([Node|Rest]) --> instruction(Node, all), (instructions(Rest, all); {Rest = []}).

instruction(Node, all) --> fn(Node); quote(Node); lit(Node); sym(Node); seq(Node); as(Node); loop(Node); if(Node); case(Node); cond(Node); mod_acc(Node); mod(Node); bool(Node); quasiquote(Node); friedquote(Node).
instruction(Node, quasi) --> instruction(Node); splice(Node); unquote(Node).
instruction(Node, fried) --> fry(Node); instruction(Node).
instruction(Node) --> instruction(Node, all).
instructions([Node|Rest], Type) --> instruction(Node, Type), (instructions(Rest, Type); {Rest = []}).

lit(lit(Value)) --> [lit_t(Value, _)].
sym(sym(Name)) --> [sym_t(Name, _)].

arg_list([Pattern|Rest]) --> pattern(Pattern), (arg_list(Rest); {Rest = []}).

block(Instructions, T)    --> ({T = s}, ['->'(_)]; ['::'(_)]), instructions(Instructions), ['end'(_)].
block([Instruction], _)   --> ['->'(_)], instruction(Instruction).

pat_list([Pattern|Rest])         --> arg_list(Pattern), ([','(_)], pat_list(Rest); {Rest = []}).

pattern(pat_wild)  --> [sym_t(V, _)], {string_codes(V, C), [0'_|_] = C}.
pattern(pat_lit(Value))     --> [lit_t(Value, _)].
pattern(pat_var(Name))     --> [sym_t(Name, _)].
pattern(pat_cons(Head, Tail)) --> ['['(_)], pat_list(Head), ['<:'(_)], pattern(Tail), [']'(_)].
pattern(pat_snoc(F, L)) --> ['['(_)], pattern(F), [':>'(_)], pat_list(L), [']'(_)].
pattern(pat_tape(L))    --> ['['(_)], (pat_list(L); {L = []}), [']'(_)].
pattern(pat_quote(Q))  --> ['('(_)], (pattern(pat_var(Q)); {Q = ""}), [')'(_)]. % extract the code inside the quote to make a function

branch(branch(Patterns, When, Instructions), case) --> arg_list(Patterns), (['when'(_)], instructions(When); {When = []}), ['->'(_)], instructions(Instructions, all).
branch(branch(Expressions, Instructions), cond) --> instructions(Expressions), ['->'(_)], instructions(Instructions, all).
branches([Branch|Rest], Type) --> (['|'(_)]; {true}), branch(Branch, Type), (['|'(_)], branches(Rest, Type); {Rest = []}).

% convert multiple fns to a single one + case
fn(fn(Name, Args, When, Body)) --> ['fn'(_)], (arg_list(Args); {Args = []}), [sym_t(Name, _)], (['when'(_)], instructions(When); {When = []}), block(Body, t).

cond(cond(Branches, Else)) --> ['cond'(_)], branches(Branches, cond), (['|'(_), '->'(_)], instructions(Else); {Else = []}), ['end'(_)].

% TODO: add multiple values and patterns support
% unify as and elemgroup
case(case(Values, Branches, Else)) --> ['case'(_)], (elem_group(Values), {Values = [_|_]}, ['->'(_)]; {Values = none}), branches(Branches, case), (['|'(_), '->'(_)], instructions(Else); {Else = []}), ['end'(_)].

fry(hole) --> [sym_t("_",_)].
fry(splice) --> [sym_t("@",_)].
% wont parse
friedquote(friedquote(Body)) --> ['.'(_), '('(_)], instructions(Body, fried), [')'(_)].

splice(splice(Elems)) --> [':'(_), '('(_)], elems(Elems), [')'(_)].
unquote(unquote(Expr)) --> [','(_)], instruction(Expr, quasi).
quasiquote(quasiquote(Body)) --> ['t'(_), '('(_)], instructions(Body, quasi), [')'(_)].

lam_body([as(Args)|Body]) --> arg_list(Args), block(Body, s).
lam_body([as(Args)|Body], diff) --> arg_list(Args), ['->'(_)], instructions(Body).
lam_bodies([Body|Bodies]) --> (['|'(_)]; {true}), lam_body(Body, diff), (['|'(_)], lam_bodies(Bodies); {Bodies = []}).

quote(named_quote(Name, Body)) --> ['lam'(_), '~'(_), sym_t(Name, _)], lam_body(Body).
quote(named_quote_case(Name, Bodies)) --> ['lam'(_), '~'(_), sym_t(Name, _)], lam_bodies(Bodies), ['end'(_)].

quote(quote(Body)) --> ['lam'(_)], lam_body(Body).
quote(quote(Body)) --> ['lam'(_)], instructions(Body), ['end'(_)].
quote(quote_case(Bodies)) --> ['lam'(_)], lam_bodies(Bodies), ['end'(_)].
quote(quote(Body)) --> ['('(_)], (instructions(Body); {Body = []}), [')'(_)].

as(as(Args)) --> ['as'(_)], (arg_list(Args); {Args = []}), ['->'(_)].

loop(loop(N, Body)) --> ['loop'(_)], (lit(N); sym(N)), block(Body, s).

if(if(Condition, If, Else)) --> ['if'(_)], (instructions(Condition); {Condition = []}), ['->'(_)], instructions(If), (['else'(_)], instructions(Else), ['end'(_)]; ['end'(_)], {Else = []}).

mod(mod(Name, Body)) --> ['mod'(_), sym_t(Name, _)], block(Body, t).
mod_acc(mod_acc(Mod, Item)) --> [sym_t(Mod, _), ':'(_), sym_t(Item, _)].
% TODO: parse imports and use

bool(lit(Bool)) --> [lit_t(yes, _), {Bool = yes}; lit_t(no, _), {Bool = no}].

elem_group([Node|Rest])     --> instruction(Node, all), (elem_group(Rest); {Rest = []}).
elems([Node|Rest])          --> elem_group(Node), ([','(_)], elems(Rest); {Rest = []}).
seq(cons(Head, Tail))       --> ['['(_)], elems(Head), ['<:'(_)], elem_group(Tail), [']'(_)].
seq(snoc(Beginning, Last))       --> ['['(_)], elem_group(Beginning), [':>'(_)], elems(Last), [']'(_)].
seq(tape(Elements))          --> ['['(_)], (elems(Elements); {Elements = []}), [']'(_)]. % circular doubly linked list



