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

instruction(Node, all) --> fn(Node); quote(Node); lit(Node); sym(Node); seq(Node); tracer; as(Node); loop(Node); if(Node); case(Node); cond(Node); mod_acc(Node); mod(Node); bool(Node).
instructions([Node|Rest], Type) --> instruction(Node, Type), (instructions(Rest, Type); {Rest = []}).

lit(lit(Value)) --> [lit_t(Value, _)].
sym(sym(Name)) --> [sym_t(Name, _)].

arg_list([Pattern|Rest]) --> pattern(Pattern), (arg_list(Rest); {Rest = []}).
lit_list([L|Rest])     --> (lit(L); sym(L)), (lit_list(Rest); {Rest = []}).
% expr_list([Expr|Rest]) --> instruction(Expr, all), (expr_list(Rest);
% {Rest = []}).

block(Instructions, T)    --> ({T = s}, [':-'(_)]; ['::'(_)]), instructions(Instructions), ['end'(_)]; ['->'(_)], instruction(Instructions, all).

pat_list([Pattern|Rest])         --> arg_list(Pattern), ([','(_)], pat_list(Rest); {Rest = []}).

pattern(pat_lit(Value))     --> [lit_t(Value, _)].
pattern(pat_var(Name))     --> [sym_t(Name, _)].
pattern(pat_cons(Head, Tail)) --> ['['(_)], pat_list(Head), ['<:'(_)], pattern(Tail), [']'(_)].
pattern(pat_snoc(F, L)) --> ['['(_)], pattern(F), [':>'(_)], pat_list(L), [']'(_)].
pattern(pat_tape(L))    --> ['['(_)], (pat_list(L); {L = []}), [']'(_)].
pattern(pat_tuple(T))   --> ['{'(_)], (pat_list(T); {T = []}), ['}'(_)].
pattern(pat_quote(Q))  --> ['('(_)], (pattern(pat_var(Q)); {Q = ""}), ['}'(_)]. % extract the code inside the quote to make a function

branch(branch(Expressions, When, Instructions), case) --> pat_list(Expressions), (['when'(_)], instructions(When); {When = []}), ['->'(_)], instructions(Instructions, all).
branch(branch(Expressions, Instructions), cond) --> instructions(Expressions), ['->'(_)], instructions(Instructions, all).
branches([Branch|Rest], Type) --> (['|'(_)]; {true}), branch(Branch, Type), (['|'(_)], branches(Rest, Type); {Rest = []}).

% convert multiple fns to a single one + case
fn(fn(Name, Args, When, Body)) --> ['fn'(_)], (arg_list(Args); {Args = []}), [sym_t(Name, _)], (['when'(_)], instructions(When); {When = []}), block(Body, t).

cond(cond(Branches, Else)) --> ['cond'(_)], branches(Branches, cond), (['|'(_), '->'(_)], instructions(Else, all); {Else = []}), ['end'(_)].

case(case(Values, Branches, Else)) --> ['case'(_)], (elems(Values), [':-'(_)]; {Values = []}), branches(Branches, case), (['|'(_), '->'(_)], instructions(Else, case); {Else = []}), ['end'(_)].

tracer --> [sym_t("$trace",_)], {trace}; [sym_t("$gtrace",_)], {gtrace}.

lam_body([as(Args)|Body]) --> arg_list(Args), block(Body, s).
lam_body([as(Args)|Body], diff) --> arg_list(Args), [':-'(_)], instructions(Body).
lam_bodies([Body|Bodies]) --> (['|'(_)]; {true}), lam_body(Body, diff), (['|'(_)], lam_bodies(Bodies); {Bodies = []}).
quote(named_quote(Name, Body)) --> ['lam'(_), '~'(_), sym_t(Name, _)], lam_body(Body).
quote(named_quote_case(Name, Bodies)) --> ['lam'(_), '~'(_), sym_t(Name, _)], lam_bodies(Bodies), ['end'(_)].

quote(quote(Body)) --> ['lam'(_)], lam_body(Body).
quote(quote(Body))  --> ['lam'(_)], instructions(Body), ['end'(_)].
quote(quote_case(Bodies)) --> ['lam'(_)], lam_bodies(Bodies), ['end'(_)].
quote(quote(Body)) --> ['('(_)], instructions(Body), [')'(_)].

as(as(Args)) --> ['as'(_)], arg_list(Args), ['->'(_)].

loop(loop(Range, Body)) --> ['loop'(_)], lit_list(Range), {Range = [_]; Range = [_,_]}, block(Body, s).

if(if(Condition, If, Else)) --> ['if'(_)], (instructions(Condition); {Condition = []}), [':-'(_)], instructions(If), (['else'(_)], instructions(Else), ['end'(_)]; ['end'(_)], {Else = []}).

mod(mod(Name, Body)) --> ['mod'(_), sym_t(Name, _)], block(Body, t).
mod_acc(mod_acc(Mod, Item)) --> [sym_t(Mod, _), ':'(_), sym_t(Item, _)].

bool(lit(Bool)) --> [lit_t(yes, _), {Bool = yes}; lit_t(no, _), {Bool = no}].

% expr(Node) --> lit(Node); sym(Node); seq(Node); lam(Node); loop(Node);
% if(Node); as(Node).

elem_group([Node|Rest])     --> instruction(Node, all), (elem_group(Rest); {Rest = []}).
elems([Node|Rest])          --> elem_group(Node), ([','(_)], elems(Rest); {Rest = []}).
seq(cons(Head, Tail))       --> ['['(_)], elems(Head), ['<:'(_)], elem_group(Tail), [']'(_)].
seq(snoc(Beginning, Last))       --> ['['(_)], elem_group(Beginning), [':>'(_)], elems(Last), [']'(_)].
seq(tape(Elements))          --> ['['(_)], (elems(Elements); {Elements = []}), [']'(_)]. % circular doubly linked list
seq(tuple(Elements))          --> ['{'(_)], (elems(Elements); {Elements = []}), ['}'(_)].


