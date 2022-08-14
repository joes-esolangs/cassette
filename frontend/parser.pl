:- module(parser, [parse/2, pparse/1]).
:- use_module(lexer).
% add pattern matching for arguments

parse(Code, AST) :-
    tokenize(Code, Out),
    instructions(AST, Out, []).

pparse(Code) :-
    parse(Code, AST),
    print_term(AST, []).

instructions([Node|Rest]) --> instruction(Node, all), (instructions(Rest, all); {Rest = []}).

instruction(Node, all) --> fn(Node); lam(Node); lit(Node); sym(Node); seq(Node); pass(Node); tracer; as(Node); loop(Node); if(Node); case(Node); cond(Node); mod_acc(Node); mod(Node); bool(Node).
instructions([Node|Rest], Type) --> instruction(Node, Type), (instructions(Rest, Type); {Rest = []}).

lit(lit(Value)) --> [lit_t(Value, _)].
sym(sym(Name)) --> [sym_t(Name, _)].

arg_list([Pattern|Rest]) --> pattern(Pattern), (arg_list(Rest); {Rest = []}).
lit_list([L|Rest])     --> (lit(L); sym(L)), (lit_list(Rest); {Rest = []}).
% expr_list([Expr|Rest]) --> instruction(Expr, all), (expr_list(Rest);
% {Rest = []}).

block(Instructions, T)    --> ({T = s}, [':-'(_)]; ['::'(_)]), instructions(Instructions), ['end'(_)]; ['->'(_)], instruction(Instructions, all).

pat_list([Pattern|Rest])         --> pattern(Pattern), ([','(_)], pat_list(Rest); {Rest = []}).
pattern(pat_lit(Value))     --> [lit_t(Value, _)].
pattern(pat_var(Name))     --> [sym_t(Name, _)].
pattern(pat_cons(Head, Tail)) --> ['['(_)], pat_list(Head), ['<:'(_)], pattern(Tail), [']'(_)].
pattern(pat_snoc(F, L)) --> ['['(_)], pattern(F), [':>'(_)], pat_list(L), [']'(_)].
pattern(pat_list(L))    --> ['['(_)], (pat_list(L); {L = []}), [']'(_)].
pattern(pat_tuple(T))   --> ['{'(_)], (pat_list(T); {T = []}), ['}'(_)].
pattern(pat_quote(Q))  --> ['('(_)], (pattern(pat_var(Q)); {Q = ""}), ['}'(_)]. % extract the code inside the quote to make a function

branch(branch(Expressions, When, Instructions), case, NoPipe) --> ({NoPipe = true}; ['|'(_)]), pat_list(Expressions), (['when'(_)], instructions(When); {When = []}), ['->'(_)], instructions(Instructions, all).
branch(branch(Expressions, Instructions), cond, NoPipe) --> ({NoPipe = true}; ['|'(_)]), instructions(Expressions), ['->'(_)], instructions(Instructions, all).
branches([Branch|Rest], Type) --> ({Pipe = true; Pipe = false}, branch(Branch, Type, Pipe)), (branches(Rest, Type); {Rest = []}).

fn(fn(Name, Args, When, Body)) --> ['fn'(_)], (arg_list(Args); {Args = []}), [sym_t(Name, _)], (['when'(_)], instructions(When); {When = []}), block(Body, t).

cond(cond(Branches, Else)) --> ['cond'(_)], branches(Branches, cond), (['|'(_), '->'(_)], instructions(Else, all); {Else = []}), ['end'(_)].

case(case(Values, Branches, Else)) --> ['case'(_)], (elems(Values); {Values = []}), [':-'(_)], branches(Branches, case), (['|'(_), '->'(_)], instructions(Else, case); {Else = []}), ['end'(_)].

tracer --> [sym_t("$trace",_)], {trace}.

lam(named_lam(Name, [as(Args)|Body])) --> ['lam'(_), '~'(_), sym_t(Name, _)], arg_list(Args), block(Body, s).

lam(quote([as(Args)|Body])) --> ['lam'(_)], arg_list(Args), block(Body, s).
lam(quote(Body))  --> ['lam'(_)], instructions(Body), ['end'(_)].
lam(quote(Body)) --> ['('(_)], instructions(Body), [')'(_)].

as(as(Args)) --> ['as'(_)], arg_list(Args), ['->'(_)].

loop(loop(Range, Body)) --> ['loop'(_)], lit_list(Range), {Range = [_]; Range = [_,_]}, block(Body, s).

if(if(Condition, If, Else)) --> ['if'(_)], (instructions(Condition); {Condition = []}), [':-'(_)], instructions(If), (['else'(_)], instructions(Else), ['end'(_)]; ['end'(_)], {Else = []}).

mod(mod(Name, Body)) --> ['mod'(_), sym_t(Name, _)], block(Body, t).
mod_acc(mod_acc(Mod, Item)) --> [sym_t(Mod, _), ':'(_), sym_t(Item, _)].

pass(lit(pass)) --> [lit_t(pass, _)].
bool(lit(Bool)) --> [lit_t(yes, _), {Bool = yes}; lit_t(no, _), {Bool = no}].

% expr(Node) --> lit(Node); sym(Node); seq(Node); lam(Node); loop(Node);
% if(Node); as(Node).

elem_group(Node)     --> instruction(Node, all).
elem_group([Node|Rest])     --> instruction(Node, all), (elem_group(Rest); {Rest = []}).
elems([Node|Rest])          --> elem_group(Node), ([','(_)], elems(Rest); {Rest = []}).
seq(cons(Head, Tail))       --> ['['(_)], elems(Head), ['<:'(_)], elem_group(Tail), [']'(_)].
seq(snoc(Beginning, Last))       --> ['['(_)], elem_group(Beginning), [':>'(_)], elems(Last), [']'(_)].
seq(lit(tape(Elements)))          --> ['['(_)], (elems(Elements); {Elements = []}), [']'(_)]. % circular doubly linked list
seq(lit(tuple(Elements)))          --> ['{'(_)], (elems(Elements); {Elements = []}), ['}'(_)].


